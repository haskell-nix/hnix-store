{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
module System.Nix.Store.Remote.Protocol
  ( WorkerOp(..)
  , simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , runStore
  , runStoreOpts
  )
where

import qualified Relude.Unsafe                 as Unsafe

import           Control.Exception              ( bracket )
import           Control.Monad.Except

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy

import           Network.Socket                 ( SockAddr(SockAddrUnix) )
import qualified Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

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


simpleOp :: WorkerOp -> MonadStore Bool
simpleOp op = simpleOpArgs op pass

simpleOpArgs :: WorkerOp -> Put -> MonadStore Bool
simpleOpArgs op args = do
  runOpArgs op args
  err <- gotError
  bool
    sockGetBool
    (do
      Error _num msg <- Unsafe.head <$> getError
      throwError $ Data.ByteString.Char8.unpack msg
    )
    err

runOp :: WorkerOp -> MonadStore ()
runOp op = runOpArgs op pass

runOpArgs :: WorkerOp -> Put -> MonadStore ()
runOpArgs op args =
  runOpArgsIO
    op
    (\encode -> encode $ toStrict $ runPut args)

runOpArgsIO
  :: WorkerOp
  -> ((Data.ByteString.ByteString -> MonadStore ()) -> MonadStore ())
  -> MonadStore ()
runOpArgsIO op encoder = do

  sockPut $ putInt $ opNum op

  soc <- asks storeSocket
  encoder (liftIO . sendAll soc)

  out <- processOutput
  modify (\(a, b) -> (a, b <> out))
  err <- gotError
  when err $ do
    Error _num msg <- Unsafe.head <$> getError
    throwError $ Data.ByteString.Char8.unpack msg

runStore :: MonadStore a -> IO (Either String a, [Logger])
runStore = runStoreOpts defaultSockPath "/nix/store"

runStoreOpts
  :: FilePath -> FilePath -> MonadStore a -> IO (Either String a, [Logger])
runStoreOpts sockPath storeRootDir code = do
  bracket (open sockPath) (Network.Socket.close . storeSocket) run
 where
  open path = do
    soc <-
      Network.Socket.socket
        Network.Socket.AF_UNIX
        Network.Socket.Stream
        0

    Network.Socket.connect soc (SockAddrUnix path)
    pure StoreConfig
        { storeSocket = soc
        , storeDir = storeRootDir
        }

  greet = do
    sockPut $ putInt workerMagic1
    soc      <- asks storeSocket
    vermagic <- liftIO $ recv soc 16
    let
      (magic2, _daemonProtoVersion) =
        flip runGet (fromStrict vermagic)
          $ (,)
            <$> (getInt :: Get Int)
            <*> (getInt :: Get Int)
    unless (magic2 == workerMagic2) $ error "Worker magic 2 mismatch"

    sockPut $ putInt protoVersion -- clientVersion
    sockPut $ putInt (0 :: Int)   -- affinity
    sockPut $ putInt (0 :: Int)   -- obsolete reserveSpace

    processOutput

  run sock =
    fmap (\(res, (_data, logs)) -> (res, logs))
      $ (`runReaderT` sock)
      $ (`runStateT` (Nothing, []))
      $ runExceptT (greet >> code)
