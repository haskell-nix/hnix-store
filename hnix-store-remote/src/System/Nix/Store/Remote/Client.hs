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
import Data.Serialize.Put (Put, runPut)
import Data.Some (Some(Some))

import qualified Data.Bool
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
import System.Nix.Store.Remote.Types.StoreConfig (HasStoreSocket, PreStoreConfig, StoreConfig, preStoreConfigToStoreConfig)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp)

-- WIP ops
import System.Nix.Hash (HashAlgo(..))
import System.Nix.StorePath (StorePathName)
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))

simpleOp
  :: ( MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     , HasProtoVersion r
     )
  => WorkerOp
  -> RemoteStoreT r m Bool
simpleOp op = simpleOpArgs op $ pure ()

simpleOpArgs
  :: ( MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     , HasProtoVersion r
     )
  => WorkerOp
  -> Put
  -> RemoteStoreT r m Bool
simpleOpArgs op args = do
  runOpArgs op args
  err <- gotError
  Data.Bool.bool
    (sockGetS $ mapErrorS RemoteStoreError_SerializerGet bool)
    (do
      -- TODO: don't use show
      getErrors >>= throwError . RemoteStoreError_Fixme . show
    )
    err

runOp
  :: ( MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     , HasProtoVersion r
     )
  => WorkerOp
  -> RemoteStoreT r m ()
runOp op = runOpArgs op $ pure ()

runOpArgs
  :: ( MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     , HasProtoVersion r
     )
  => WorkerOp
  -> Put
  -> RemoteStoreT r m ()
runOpArgs op args =
  runOpArgsIO
    op
    (\encode -> encode $ runPut args)

runOpArgsIO
  :: ( MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     , HasProtoVersion r
     )
  => WorkerOp
  -> ((Data.ByteString.ByteString -> RemoteStoreT r m ())
       -> RemoteStoreT r m ()
     )
  -> RemoteStoreT r m ()
runOpArgsIO op encoder = do
  sockPutS (mapErrorS RemoteStoreError_SerializerPut enum) op

  soc <- getStoreSocket
  encoder (liftIO . Network.Socket.ByteString.sendAll soc)

  out <- processOutput
  appendLogs out
  err <- gotError
  when err $ do
    -- TODO: don't use show
    getErrors >>= throwError . RemoteStoreError_Fixme . show

doReq
  :: ( MonadIO m
     , StoreReply a
     )
  => StoreRequest a
  -> RemoteStoreT StoreConfig m a
doReq = \case
  x -> do
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
    out <- processOutput
    appendLogs out
    sockGetS (mapErrorS RemoteStoreError_SerializerGet getReply)

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

-- | Pack `Nar` and add it to the store.
addToStore
  :: MonadIO m
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource IO         -- ^ Provide nar stream
  -> FileIngestionMethod  -- ^ Add target directory recursively
  -> Some HashAlgo        -- ^
  -> RepairMode           -- ^ Only used by local store backend
  -> RemoteStoreT StoreConfig m StorePath
addToStore name source method hashAlgo repair = do
  Control.Monad.when
    (repair == RepairMode_DoRepair)
    $ throwError RemoteStoreError_RapairNotSupportedByRemoteStore

  setNarSource source
  doReq (AddToStore name method hashAlgo repair)

isValidPath :: MonadIO m => StorePath -> RemoteStoreT StoreConfig m Bool
isValidPath = doReq . IsValidPath

-- TOOD: want this, but Logger.processOutput is fixed to RemoteStoreT r m
--isValidPath' :: MonadRemoteStore m => StorePath -> m Bool
--isValidPath' = doReq . IsValidPath

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

      logs <-
        mapStoreConfig
          (preStoreConfigToStoreConfig minimumCommonVersion)
          processOutput

      pure Handshake
        { handshakeNixVersion = daemonNixVersion
        , handshakeTrust = remoteTrustsUs
        , handshakeProtoVersion = minimumCommonVersion
        , handshakeRemoteProtoVersion = daemonVersion
        , handshakeLogs = logs
        }
