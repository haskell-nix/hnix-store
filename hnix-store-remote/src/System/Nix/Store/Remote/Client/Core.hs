module System.Nix.Store.Remote.Client.Core
  ( Run
  , runStoreSocket
  , doReq
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.DList (DList)
import Data.Some (Some(Some))
import System.Nix.Nar (NarSource)
import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
  ( MonadRemoteStore
  , RemoteStoreError(..)
  , RemoteStoreT
  , runRemoteStoreT
  , mapStoreConfig
  , takeNarSource
  , getStoreSocket
  )
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
import System.Nix.Store.Remote.Serializer
  ( bool
  , int
  , mapErrorS
  , protoVersion
  , storeRequest
  , text
  , trustedFlag
  , workerMagic
  )
import System.Nix.Store.Remote.Types.Handshake (ClientHandshakeInput(..), ClientHandshakeOutput(..))
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..), ourProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (PreStoreConfig, StoreConfig, preStoreConfigToStoreConfig)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.StoreReply (StoreReply(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))

import qualified Network.Socket.ByteString

type Run m a = m (Either RemoteStoreError a, DList Logger)

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
        , clientHandshakeOutputLeastCommonVersion = leastCommonVersion
        , clientHandshakeOutputServerVersion = daemonVersion
        }
