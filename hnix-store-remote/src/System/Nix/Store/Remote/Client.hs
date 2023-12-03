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
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Serialize.Put (Put, runPut)

import qualified Data.Bool
import qualified Data.ByteString
import qualified Network.Socket.ByteString

import System.Nix.StorePath (HasStoreDir(..))
import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
import System.Nix.Store.Remote.Serializer (bool, enum, int, mapErrorS, protoVersion, text, trustedFlag, workerMagic)
import System.Nix.Store.Remote.Types.Handshake (Handshake(..))
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion, ProtoVersion(..), ourProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (HasStoreSocket, PreStoreConfig, StoreConfig, preStoreConfigToStoreConfig)
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp)

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
