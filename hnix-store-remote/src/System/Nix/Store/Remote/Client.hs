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
import Control.Monad.IO.Class (liftIO)
import Data.Serialize.Put (Put, runPut)

import qualified Data.Bool
import qualified Data.ByteString
import qualified Network.Socket.ByteString

import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
import System.Nix.Store.Remote.Serializer (bool, enum, int, mapErrorS, protoVersion, text, workerMagic)
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..), ourProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (PreStoreConfig(..), StoreConfig(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp)

type Run a = IO (Either RemoteStoreError a, [Logger])

simpleOp :: WorkerOp -> MonadRemoteStore Bool
simpleOp op = simpleOpArgs op $ pure ()

simpleOpArgs :: WorkerOp -> Put -> MonadRemoteStore Bool
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

runOp :: WorkerOp -> MonadRemoteStore ()
runOp op = runOpArgs op $ pure ()

runOpArgs :: WorkerOp -> Put -> MonadRemoteStore ()
runOpArgs op args =
  runOpArgsIO
    op
    (\encode -> encode $ runPut args)

runOpArgsIO
  :: WorkerOp
  -> ((Data.ByteString.ByteString -> MonadRemoteStore ())
       -> MonadRemoteStore ()
     )
  -> MonadRemoteStore ()
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

runStoreSocket
  :: PreStoreConfig
  -> MonadRemoteStore a
  -> Run a
runStoreSocket preStoreConfig code =
  runRemoteStoreT preStoreConfig $ do
    pv <- greet
    mapStoreConfig
      (\(PreStoreConfig a b) -> StoreConfig a pv b)
      code

  where
    greet :: MonadRemoteStoreHandshake ProtoVersion
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

      when (daemonVersion >= ProtoVersion 1 14)
        $ sockPutS int (0 :: Int) -- affinity, obsolete

      when (daemonVersion >= ProtoVersion 1 11) $ do
        sockPutS
          (mapErrorS RemoteStoreError_SerializerPut bool)
          False -- reserveSpace, obsolete

      -- not quite right, should be min of the two
      -- as well as two ^ above
      when (ourProtoVersion >= ProtoVersion 1 33) $ do
        -- If we were buffering I/O, we would flush the output here.
        _daemonNixVersion <-
          sockGetS
          $ mapErrorS
              RemoteStoreError_SerializerGet
              text
        return ()

      -- TODO do something with it
      -- TODO patter match better
      _ <- mapStoreConfig
            (\(PreStoreConfig a b) -> StoreConfig a ourProtoVersion b)
            processOutput

      -- TODO should be minimum of
      -- ourProtoVersion vs daemonVersion
      pure ourProtoVersion
