{-# LANGUAGE OverloadedStrings #-}

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
  , isValidPath
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Data.Serialize.Put (Put, runPut)
import Data.Some (Some(Some))

import qualified Data.ByteString
import qualified Network.Socket.ByteString

import System.Nix.Nar (NarSource)
import System.Nix.StorePath (HasStoreDir(..), StorePath)
import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
--import System.Nix.Store.Remote.Serializer (NixSerializer, SError, bool, enum, int, mapErrorS, protoVersion, storeRequest, text, trustedFlag, workerMagic)
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.Handshake (Handshake(..))
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion, ProtoVersion(..), ourProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (PreStoreConfig, StoreConfig, preStoreConfigToStoreConfig)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp)

-- WIP ops
import System.Nix.Hash (HashAlgo(..))
import System.Nix.StorePath (StorePathName)
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))

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
    cfg <- ask
    _ <- runRemoteStoreT cfg $
      sockPutS (mapErrorS RemoteStoreError_SerializerPut storeRequest) (Some x)
    case x of
      AddToStore {} -> do

        ms <- takeNarSource
        case ms of
          Just (stream :: NarSource IO) -> do
            soc <- getStoreSocket
            liftIO $ stream $ Network.Socket.ByteString.sendAll soc
          Nothing -> throwError RemoteStoreError_NoNarSourceProvided

      _ -> pure ()

    --either (throwError @RemoteStoreError @m) (\() -> pure ()) . fst
    --       <$> runRemoteStoreT cfg processOutput
    processOutput
    --either throwError pure . fst <$> runRemoteStoreT cfg $
    eres <- runRemoteStoreT cfg $
      sockGetS (mapErrorS RemoteStoreError_SerializerGet (getReply @a))

    case eres of
      (Left e, _logs) -> throwError e
      (Right res, _logs) -> pure res

class StoreReply a where
  getReply
    :: ( HasStoreDir r
       , HasProtoVersion r
       )
    => NixSerializer r SError a

instance StoreReply Bool where
  getReply = bool

instance StoreReply StorePath where
  getReply = storePath

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

--isValidPath :: MonadIO m => StorePath -> RemoteStoreT StoreConfig m Bool
--isValidPath = doReq . IsValidPath

-- TOOD: want this, but Logger.processOutput is fixed to RemoteStoreT r m
isValidPath :: MonadRemoteStore m => StorePath -> m Bool
isValidPath = doReq . IsValidPath

type Run m a = m (Either RemoteStoreError a, [Logger])

runStoreSocket
  :: ( Monad m
     , MonadIO m
     )
  => PreStoreConfig
  -> RemoteStoreT StoreConfig m a
  -> Run m a
runStoreSocket preStoreConfig code =
  runRemoteStoreT preStoreConfig $ do
    Handshake{..} <- greet
    mapStoreConfig
      (preStoreConfigToStoreConfig handshakeProtoVersion)
      code

  where
    greet
      :: MonadIO m
      => RemoteStoreT PreStoreConfig m Handshake
    greet = do

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

      sockPutS protoVersion ourProtoVersion

      let minimumCommonVersion = min daemonVersion ourProtoVersion

      when (minimumCommonVersion >= ProtoVersion 1 14)
        $ sockPutS int (0 :: Int) -- affinity, obsolete

      when (minimumCommonVersion >= ProtoVersion 1 11) $ do
        sockPutS
          (mapErrorS RemoteStoreError_SerializerPut bool)
          False -- reserveSpace, obsolete

      daemonNixVersion <- if minimumCommonVersion >= ProtoVersion 1 33
        then do
          -- If we were buffering I/O, we would flush the output here.
          txtVer <-
            sockGetS
              $ mapErrorS
                  RemoteStoreError_SerializerGet
                  text
          pure $ Just txtVer
        else pure Nothing

      remoteTrustsUs <- if minimumCommonVersion >= ProtoVersion 1 35
        then do
          sockGetS
            $ mapErrorS RemoteStoreError_SerializerHandshake trustedFlag
        else pure Nothing

      mapStoreConfig
        (preStoreConfigToStoreConfig minimumCommonVersion)
        processOutput

      pure Handshake
        { handshakeNixVersion = daemonNixVersion
        , handshakeTrust = remoteTrustsUs
        , handshakeProtoVersion = minimumCommonVersion
        , handshakeRemoteProtoVersion = daemonVersion
        }
