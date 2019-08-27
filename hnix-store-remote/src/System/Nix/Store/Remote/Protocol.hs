{-# LANGUAGE TypeApplications #-}
module System.Nix.Store.Remote.Protocol (
    WorkerOp(..)
  , runOp
  , runOp_
  , runOpArgs
  , runOpArgs_
  , runStore
  , runStore_
  ) where

import           Control.Exception         (SomeException, bracket, catch, displayException)
import           Control.Monad.Except      (throwError, runExceptT)
import           Control.Monad.Reader

import           Data.Binary.Put
import           Data.Binary.Get

import           Network.Socket

import           Pipes
import qualified Pipes.Prelude as Pipes

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

runOp :: WorkerOp -> Get a -> MonadStore a
runOp op result = runOpArgs op mempty result

runOp_ :: WorkerOp -> MonadStore ()
runOp_ op = runOp op (skip 8)

runOpArgs :: WorkerOp -> Put -> Get a -> MonadStore a
runOpArgs op args result = do
  sockPut $ do
    putInt (opNum op)
    args
  streamLogs
  sockGet result

runOpArgs_ :: WorkerOp -> Put -> MonadStore ()
runOpArgs_ op args = runOpArgs op args (skip 8)

runStore :: Consumer Logger IO (Either Error a) -> MonadStore a -> IO (Either Error a)
runStore sink code =
  bracket (open sockPath) close run `catch` onException
  where
    open path = do
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix path)
      return sock

    greet = do
      sockPut $ putInt workerMagic1

      magic2 <- sockGetInt
      _ <- sockGetInt -- daemonVersion

      unless (magic2 == workerMagic2) $
        throwError (ConnError "Worker magic 2 mismatch")

      sockPut $ putInt protoVersion -- clientVersion
      sockPut $ putInt (0 :: Int)   -- affinity
      sockPut $ putInt (0 :: Int)   -- obsolete reserveSpace

      streamLogs -- receive startup error messages, if any

    run sock =
      let producer =
            runExceptT $ do
              greet
              code
          effect = producer >-> hoist liftIO sink
      in  runReaderT (runEffect effect) sock

    onException :: SomeException -> IO (Either Error a)
    onException = return . Left . ConnError . displayException

runStore_ :: MonadStore a -> IO (Either Error a)
runStore_ = runStore Pipes.drain
