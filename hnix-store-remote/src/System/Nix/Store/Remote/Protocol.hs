module System.Nix.Store.Remote.Protocol (
    WorkerOp(..)
  , simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runStore) where

import           Control.Exception         (bracket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS

import           Network.Socket            hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString (recv)

import           System.Nix.Store.Remote.Logger
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Util
import           System.Nix.Util

protoVersion :: Int
protoVersion = 0x115
-- major protoVersion & 0xFF00
-- minor ..           & 0x00FF

workerMagic1 :: Int
workerMagic1 = 0x6e697863
workerMagic2 :: Int
workerMagic2 = 0x6478696f

sockPath :: String
sockPath = "/nix/var/nix/daemon-socket/socket"

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


simpleOp :: WorkerOp -> MonadStore Bool
simpleOp op = do
  simpleOpArgs op $ return ()

simpleOpArgs :: WorkerOp -> Put -> MonadStore Bool
simpleOpArgs op args = do
  runOpArgs op args
  err <- gotError
  case err of
    True -> do
      Error _num msg <- head <$> getError
      throwError $ BSC.unpack $ LBS.toStrict msg
    False -> do
      sockGetBool

runOp :: WorkerOp -> MonadStore ()
runOp op = runOpArgs op $ return ()

runOpArgs :: WorkerOp -> Put -> MonadStore ()
runOpArgs op args = do

  -- Temporary hack for printing the messages destined for nix-daemon socket
  when False $
    liftIO $ LBS.writeFile "mytestfile2" $ runPut $ do
      putInt $ opNum op
      args

  sockPut $ do
    putInt $ opNum op
    args

  out <- processOutput
  modify (++out)
  err <- gotError
  when err $ do
    Error _num msg <- head <$> getError
    throwError $ BSC.unpack $ LBS.toStrict msg

runStore :: MonadStore a -> IO (Either String a, [Logger])
runStore code = do
  bracket (open sockPath) close run
  where
    open path = do
      soc <- socket AF_UNIX Stream 0
      connect soc (SockAddrUnix path)
      return soc
    greet = do
      sockPut $ putInt workerMagic1
      soc <- ask
      vermagic <- liftIO $ recv soc 16
      let (magic2, _daemonProtoVersion) = flip runGet (LBS.fromStrict vermagic) $ (,) <$> (getInt :: Get Int) <*> (getInt :: Get Int)
      unless (magic2 == workerMagic2) $ error "Worker magic 2 mismatch"

      sockPut $ putInt protoVersion -- clientVersion
      sockPut $ putInt (0 :: Int)   -- affinity
      sockPut $ putInt (0 :: Int)   -- obsolete reserveSpace

      processOutput

    run sock =
      flip runReaderT sock $ flip runStateT [] $ runExceptT (greet >> code)
