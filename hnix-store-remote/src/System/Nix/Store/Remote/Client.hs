module System.Nix.Store.Remote.Client
  ( Run
  , simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , runStoreSocket
  , ourProtoVersion
  , doReq
  , addToStore
  , buildDerivation
  , isValidPath
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.DList (DList)
import Data.Serialize.Put (Put, runPut)
import Data.Some (Some(Some))

import qualified Data.ByteString
import qualified Network.Socket.ByteString

import System.Nix.Hash (HashAlgo(..))
import System.Nix.Nar (NarSource)
import System.Nix.StorePath (StorePath, StorePathName)
import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
import System.Nix.Store.Remote.Serializer (bool, enum, int, mapErrorS, protoVersion, storeRequest, text, trustedFlag, workerMagic)
import System.Nix.Store.Remote.Types.Handshake (ClientHandshakeInput(..), ClientHandshakeOutput(..))
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..), ourProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (PreStoreConfig, StoreConfig, preStoreConfigToStoreConfig)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.StoreReply (StoreReply(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp)
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))

import Data.Text
import System.Nix.Build
import System.Nix.Derivation (Derivation)

simpleOp
  :: MonadRemoteStore m
  => WorkerOp
  -> m Bool
simpleOp op = simpleOpArgs op $ pure ()

simpleOpArgs
  :: MonadRemoteStore m
  => WorkerOp
  -> Put
  -> m Bool
simpleOpArgs op args = do
  runOpArgs op args
  errored <- gotError
  if errored
  then throwError RemoteStoreError_OperationFailed
  else sockGetS $ mapErrorS RemoteStoreError_SerializerGet bool

runOp
  :: MonadRemoteStore m
  => WorkerOp
  -> m ()
runOp op = runOpArgs op $ pure ()

runOpArgs
  :: MonadRemoteStore m
  => WorkerOp
  -> Put
  -> m ()
runOpArgs op args =
  runOpArgsIO
    op
    (\encode -> encode $ runPut args)

runOpArgsIO
  :: MonadRemoteStore m
  => WorkerOp
  -> ((Data.ByteString.ByteString -> m ())
       -> m ()
     )
  -> m ()
runOpArgsIO op encoder = do
  sockPutS (mapErrorS RemoteStoreError_SerializerPut enum) op

  soc <- getStoreSocket
  encoder (liftIO . Network.Socket.ByteString.sendAll soc)

  processOutput

-- | Perform @StoreRequest@
doReq
  :: forall m a
   . ( MonadIO m
     , MonadRemoteStore m
     , StoreReply a
     , Show a
     )
  => StoreRequest a
  -> m a
doReq = \case
  x -> do
    sockPutS
      (mapErrorS
        RemoteStoreError_SerializerRequest
          storeRequest
      )
      (Some x)

    case x of
      AddToStore {} -> do

        ms <- takeNarSource
        case ms of
          Just (stream :: NarSource IO) -> do
            soc <- getStoreSocket
            liftIO
              $ stream
              $ Network.Socket.ByteString.sendAll soc
          Nothing ->
            throwError
              RemoteStoreError_NoNarSourceProvided

      _ -> pure ()

    processOutput
    sockGetS
      (mapErrorS RemoteStoreError_SerializerReply
        $ getReplyS @a
      )

-- | Add `NarSource` to the store
addToStore
  :: MonadRemoteStore m
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource IO         -- ^ Provide nar stream
  -> FileIngestionMethod  -- ^ Add target directory recursively
  -> Some HashAlgo        -- ^
  -> RepairMode           -- ^ Only used by local store backend
  -> m StorePath
addToStore name source method hashAlgo repair = do
  Control.Monad.when
    (repair == RepairMode_DoRepair)
    $ throwError RemoteStoreError_RapairNotSupportedByRemoteStore

  setNarSource source
  doReq (AddToStore name method hashAlgo repair)

buildDerivation
  :: MonadRemoteStore m
  => StorePath
  -> Derivation StorePath Text
  -> BuildMode
  -> m BuildResult
buildDerivation a b c = doReq (BuildDerivation a b c)

--isValidPath :: MonadIO m => StorePath -> RemoteStoreT StoreConfig m Bool
--isValidPath = doReq . IsValidPath

-- TOOD: want this, but Logger.processOutput is fixed to RemoteStoreT r m
isValidPath :: MonadRemoteStore m => StorePath -> m Bool
isValidPath = doReq . IsValidPath

type Run m a = m (Either RemoteStoreError a, DList Logger)

runStoreSocket
  :: ( Monad m
     , MonadIO m
     )
  => PreStoreConfig
  -> RemoteStoreT StoreConfig m a
  -> Run m a
runStoreSocket preStoreConfig code =
  runRemoteStoreT preStoreConfig $ do
    ClientHandshakeOutput{..}
      <- greet
          ClientHandshakeInput
          { clientHandshakeInputOurVersion = ourProtoVersion
          }

    mapStoreConfig
      (preStoreConfigToStoreConfig
        clientHandshakeOutputLeastCommonVerison)
      code

  where
    greet
      :: MonadIO m
      => ClientHandshakeInput
      -> RemoteStoreT PreStoreConfig m ClientHandshakeOutput
    greet ClientHandshakeInput{..} = do

      sockPutS
        (mapErrorS
          RemoteStoreError_SerializerHandshake
          workerMagic
        )
        WorkerMagic_One

      magic <-
        sockGetS
        $ mapErrorS
            RemoteStoreError_SerializerHandshake
            workerMagic

      unless
        (magic == WorkerMagic_Two)
        $ throwError RemoteStoreError_WorkerMagic2Mismatch

      daemonVersion <- sockGetS protoVersion

      when (daemonVersion < ProtoVersion 1 10)
        $ throwError RemoteStoreError_ClientVersionTooOld

      sockPutS protoVersion clientHandshakeInputOurVersion

      let leastCommonVersion = min daemonVersion ourProtoVersion

      when (leastCommonVersion >= ProtoVersion 1 14)
        $ sockPutS int (0 :: Int) -- affinity, obsolete

      when (leastCommonVersion >= ProtoVersion 1 11) $ do
        sockPutS
          (mapErrorS RemoteStoreError_SerializerPut bool)
          False -- reserveSpace, obsolete

      daemonNixVersion <- if leastCommonVersion >= ProtoVersion 1 33
        then do
          -- If we were buffering I/O, we would flush the output here.
          txtVer <-
            sockGetS
              $ mapErrorS
                  RemoteStoreError_SerializerGet
                  text
          pure $ Just txtVer
        else pure Nothing

      remoteTrustsUs <- if leastCommonVersion >= ProtoVersion 1 35
        then do
          sockGetS
            $ mapErrorS RemoteStoreError_SerializerHandshake trustedFlag
        else pure Nothing

      mapStoreConfig
        (preStoreConfigToStoreConfig leastCommonVersion)
        processOutput

      pure ClientHandshakeOutput
        { clientHandshakeOutputNixVersion = daemonNixVersion
        , clientHandshakeOutputTrust = remoteTrustsUs
        , clientHandshakeOutputLeastCommonVerison = leastCommonVersion
        , clientHandshakeOutputServerVersion = daemonVersion
        }
