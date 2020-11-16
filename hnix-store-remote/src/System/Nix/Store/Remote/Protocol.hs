{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Nix.Store.Remote.Protocol (
    WorkerOp(..)
  , simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , runStore
  , runStoreOpts) where

import           Control.Exception         (bracket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy

import           Network.Socket            (SockAddr(SockAddrUnix))
import qualified Network.Socket
import           Network.Socket.ByteString (recv, sendAll)

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Logger
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Util

protoVersion :: Int
protoVersion = 0x115
-- major protoVersion & 0xFF00
-- minor ..           & 0x00FF

workerMagic1 :: Int
workerMagic1 = 0x6e697863
workerMagic2 :: Int
workerMagic2 = 0x6478696f

defaultSockPath :: String
defaultSockPath = "/nix/var/nix/daemon-socket/socket"

data WorkerOp =
    IsValidPath
  | HasSubstitutes
  | QueryReferrers
  | AddToStore
  | AddTextToStore
  | BuildPaths
  | EnsurePath
  | AddTempRoot
  | AddIndirectRoot
  | SyncWithGC
  | FindRoots
  | SetOptions
  | CollectGarbage
  | QuerySubstitutablePathInfo
  | QueryDerivationOutputs
  | QueryAllValidPaths
  | QueryFailedPaths
  | ClearFailedPaths
  | QueryPathInfo
  | QueryDerivationOutputNames
  | QueryPathFromHashPart
  | QuerySubstitutablePathInfos
  | QueryValidPaths
  | QuerySubstitutablePaths
  | QueryValidDerivers
  | OptimiseStore
  | VerifyStore
  | BuildDerivation
  | AddSignatures
  | NarFromPath
  | AddToStoreNar
  | QueryMissing
  deriving (Eq, Ord, Show)

opNum :: WorkerOp -> Int
opNum IsValidPath                 = 1
opNum HasSubstitutes              = 3
opNum QueryReferrers              = 6
opNum AddToStore                  = 7
opNum AddTextToStore              = 8
opNum BuildPaths                  = 9
opNum EnsurePath                  = 10
opNum AddTempRoot                 = 11
opNum AddIndirectRoot             = 12
opNum SyncWithGC                  = 13
opNum FindRoots                   = 14
opNum SetOptions                  = 19
opNum CollectGarbage              = 20
opNum QuerySubstitutablePathInfo  = 21
opNum QueryDerivationOutputs      = 22
opNum QueryAllValidPaths          = 23
opNum QueryFailedPaths            = 24
opNum ClearFailedPaths            = 25
opNum QueryPathInfo               = 26
opNum QueryDerivationOutputNames  = 28
opNum QueryPathFromHashPart       = 29
opNum QuerySubstitutablePathInfos = 30
opNum QueryValidPaths             = 31
opNum QuerySubstitutablePaths     = 32
opNum QueryValidDerivers          = 33
opNum OptimiseStore               = 34
opNum VerifyStore                 = 35
opNum BuildDerivation             = 36
opNum AddSignatures               = 37
opNum NarFromPath                 = 38
opNum AddToStoreNar               = 39
opNum QueryMissing                = 40


simpleOp :: (MonadIO m) => WorkerOp -> RemoteStoreT m Bool
simpleOp op = do
  simpleOpArgs op $ return ()

simpleOpArgs :: (MonadIO m) => WorkerOp -> Put -> RemoteStoreT m Bool
simpleOpArgs op args = do
  runOpArgs op args
  err <- gotError
  case err of
    True -> do
      err  <- head <$> getError
      case err of
        Error _num msg -> throwError $ Data.ByteString.Char8.unpack msg
        _ -> throwError $ "Well, it should really be an error by now"
    False -> do
      sockGetBool

runOp :: (MonadIO m) => WorkerOp -> RemoteStoreT m ()
runOp op = runOpArgs op $ return ()

runOpArgs :: (MonadIO m, MonadRemoteStore m) => WorkerOp -> Put -> m ()
runOpArgs op args = runOpArgsIO op (\encode -> encode $ Data.ByteString.Lazy.toStrict $ runPut args)

runOpArgsIO
    :: forall m. (MonadIO m, MonadRemoteStore m)
    => WorkerOp
    -> ((Data.ByteString.ByteString -> m ()) -> m ())
    -> m ()
runOpArgsIO op encoder = do

  sockPut $ do
    putInt $ opNum op

  soc <- getSocket
  encoder (liftIO . sendAll soc)

  out <- processOutput
  setLog . (++out) =<< getLog
  err <- gotError
  when err $ do
    err  <- head <$> getError
    case err of
      Error _num msg -> throwError $ Data.ByteString.Char8.unpack msg
      _ -> throwError $ "Well, it should really be an error by now"


runStore :: (MonadIO m, MonadBaseControl IO m) => RemoteStoreT m a -> m (Either String a, [Logger])
runStore = runStoreOpts defaultSockPath "/nix/store"

runStoreOpts :: (MonadIO m, MonadBaseControl IO m) => FilePath -> FilePath -> RemoteStoreT m a -> m (Either String a, [Logger])
runStoreOpts sockPath storeRootDir code = do
  liftBaseOp (bracket (open sockPath) (Network.Socket.close . storeSocket)) run
  where
    open path = do
      soc <-
        Network.Socket.socket
          Network.Socket.AF_UNIX
          Network.Socket.Stream
          0

      Network.Socket.connect soc (SockAddrUnix path)
      return $ StoreConfig { storeSocket = soc
                           , storeDir    = storeRootDir }

    greet :: MonadIO m => RemoteStoreT m [Logger]
    greet = do
      sockPut $ putInt workerMagic1
      soc <- storeSocket <$> RemoteStore ask
      vermagic <- liftIO $ recv soc 16
      let (magic2, _daemonProtoVersion) =
            flip runGet (Data.ByteString.Lazy.fromStrict vermagic)
            $ (,) <$> (getInt :: Get Int)
                  <*> (getInt :: Get Int)
      unless (magic2 == workerMagic2) $ error "Worker magic 2 mismatch"

      sockPut $ putInt protoVersion -- clientVersion
      sockPut $ putInt (0 :: Int)   -- affinity
      sockPut $ putInt (0 :: Int)   -- obsolete reserveSpace

      processOutput

    run config =
      fmap (\(res, state) -> (res, logs state))
        $ flip runReaderT config
        $ flip runStateT (StoreState [] Nothing)
        $ runExceptT
        $ unStore (greet >> code)
