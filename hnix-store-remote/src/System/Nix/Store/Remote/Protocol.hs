{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Store.Remote.Protocol
  ( WorkerOp(..)
  , simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , runStore
  , runStoreOpts
  , runStoreOptsTCP
  , runStoreOpts'

  , ourProtoVersion
  ) where

import qualified Control.Monad
import Control.Exception              ( bracket )
import Control.Monad.Except
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State.Strict

import Data.Default.Class (Default(def))
import qualified Data.Bool
import Data.Serialize.Get
import Data.Serialize.Put
import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Network.Socket (SockAddr(SockAddrUnix))
import qualified Network.Socket as S
import Network.Socket.ByteString (recv, sendAll)

import System.Nix.StorePath (StoreDir(..))
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Logger
import System.Nix.Store.Remote.Types hiding (protoVersion)
import System.Nix.Store.Remote.Util

protoVersion :: Int
protoVersion = 0x115
-- major protoVersion & 0xFF00
-- minor ..           & 0x00FF

ourProtoVersion :: ProtoVersion
ourProtoVersion = ProtoVersion
  { protoVersion_major = 1
  , protoVersion_minor = 35
  }

workerMagic1 :: Int
workerMagic1 = 0x6e697863
workerMagic2 :: Int
workerMagic2 = 0x6478696f

defaultSockPath :: String
defaultSockPath = "/nix/var/nix/daemon-socket/socket"

-- | Worker opcode
--
-- This type has gaps filled in so that the GHC builtin
-- Enum instance lands on the right values.
data WorkerOp
  = Reserved_0__ -- 0
  | IsValidPath -- 1
  | Reserved_2__ -- 2
  | HasSubstitutes -- 3
  | QueryPathHash -- 4 // obsolete
  | QueryReferences --  5 // obsolete
  | QueryReferrers --  6
  | AddToStore --  7
  | AddTextToStore --  8 // obsolete since 1.25, Nix 3.0. Use wopAddToStore
  | BuildPaths --  9
  | EnsurePath --  10 0xa
  | AddTempRoot --  11 0xb
  | AddIndirectRoot --  12 0xc
  | SyncWithGC --  13 0xd
  | FindRoots --  14 0xe
  | Reserved_15__ -- 15 0xf
  | ExportPath --  16 0x10 // obsolete
  | Reserved_17__ -- 17 0x11
  | QueryDeriver --  18 0x12 // obsolete
  | SetOptions --  19 0x13
  | CollectGarbage --  20 0x14
  | QuerySubstitutablePathInfo --  21 0x15
  | QueryDerivationOutputs --  22 0x16 // obsolete
  | QueryAllValidPaths --  23 0x17
  | QueryFailedPaths --  24 0x18
  | ClearFailedPaths --  25 0x19
  | QueryPathInfo --  26 0x1a
  | ImportPaths --  27 0x1b // obsolete
  | QueryDerivationOutputNames --  28 0x1c // obsolete
  | QueryPathFromHashPart --  29 0x1d
  | QuerySubstitutablePathInfos --  30 0x1e
  | QueryValidPaths --  31 0x1f
  | QuerySubstitutablePaths --  32 0x20
  | QueryValidDerivers --  33 0x21
  | OptimiseStore --  34 0x22
  | VerifyStore --  35 0x23
  | BuildDerivation --  36 0x24
  | AddSignatures --  37 0x25
  | NarFromPath --  38 0x26
  | AddToStoreNar --  39 0x27
  | QueryMissing --  40 0x28
  | QueryDerivationOutputMap --  41 0x29
  | RegisterDrvOutput --  42 0x2a
  | QueryRealisation --  43 0x2b
  | AddMultipleToStore --  44 0x2c
  | AddBuildLog --  45 0x2d
  | BuildPathsWithResults --  46 0x2e
  deriving (Bounded, Eq, Enum, Ord, Show, Read)

simpleOp :: WorkerOp -> MonadStore Bool
simpleOp op = simpleOpArgs op $ pure ()

simpleOpArgs :: WorkerOp -> Put -> MonadStore Bool
simpleOpArgs op args = do
  runOpArgs op args
  err <- gotError
  Data.Bool.bool
    sockGetBool
    (do
      Error _num msg <- head <$> getError
      throwError $ Data.ByteString.Char8.unpack msg
    )
    err

runOp :: WorkerOp -> MonadStore ()
runOp op = runOpArgs op $ pure ()

runOpArgs :: WorkerOp -> Put -> MonadStore ()
runOpArgs op args =
  runOpArgsIO
    op
    (\encode -> encode $ runPut args)

runOpArgsIO
  :: WorkerOp
  -> ((Data.ByteString.ByteString -> MonadStore ()) -> MonadStore ())
  -> MonadStore ()
runOpArgsIO op encoder = do

  sockPut $ putEnum op

  soc <- asks storeSocket
  encoder (liftIO . sendAll soc)

  out <- processOutput
  modify (\(a, b) -> (a, b <> out))
  err <- gotError
  Control.Monad.when err $ do
    Error _num msg <- head <$> getError
    throwError $ Data.ByteString.Char8.unpack msg

runStore :: MonadStore a -> IO (Either String a, [Logger])
runStore = runStoreOpts defaultSockPath def

runStoreOpts
  :: FilePath -> StoreDir -> MonadStore a -> IO (Either String a, [Logger])
runStoreOpts path = runStoreOpts' S.AF_UNIX (SockAddrUnix path)

runStoreOptsTCP
  :: String -> Int -> StoreDir -> MonadStore a -> IO (Either String a, [Logger])
runStoreOptsTCP host port storeRootDir code = do
  S.getAddrInfo (Just S.defaultHints) (Just host) (Just $ show port) >>= \case
    (sockAddr:_) -> runStoreOpts' (S.addrFamily sockAddr) (S.addrAddress sockAddr) storeRootDir code
    _ -> pure (Left "Couldn't resolve host and port with getAddrInfo.", [])

runStoreOpts'
  :: S.Family -> S.SockAddr -> StoreDir -> MonadStore a -> IO (Either String a, [Logger])
runStoreOpts' sockFamily sockAddr storeRootDir code =
  bracket open (S.close . storeSocket) run

 where
  open = do
    soc <- S.socket sockFamily S.Stream 0
    S.connect soc sockAddr
    pure StoreConfig
        { storeSocket = soc
        , storeDir = storeRootDir
        }

  greet = do
    sockPut $ putInt workerMagic1
    soc      <- asks storeSocket
    vermagic <- liftIO $ recv soc 16
    let
      eres =
        flip runGet vermagic
          $ (,)
            <$> (getInt :: Get Int)
            <*> (getInt :: Get Int)

    case eres of
      Left err -> error $ "Error parsing vermagic " ++ err
      Right (magic2, _daemonProtoVersion) -> do
        Control.Monad.unless (magic2 == workerMagic2) $ error "Worker magic 2 mismatch"

    sockPut $ putInt protoVersion -- clientVersion
    sockPut $ putInt (0 :: Int)   -- affinity
    sockPut $ putInt (0 :: Int)   -- obsolete reserveSpace

    processOutput

  run sock =
    fmap (\(res, (_data, logs)) -> (res, logs))
      $ (`runReaderT` sock)
      $ (`runStateT` (Nothing, []))
      $ runExceptT (greet >> code)
