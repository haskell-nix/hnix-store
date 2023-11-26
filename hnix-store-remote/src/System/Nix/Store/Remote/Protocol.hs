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
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Socket
import System.Nix.Store.Remote.Types

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

simpleOp :: WorkerOp -> MonadStore Bool
simpleOp op = simpleOpArgs op $ pure ()

simpleOpArgs :: WorkerOp -> Put -> MonadStore Bool
simpleOpArgs op args = do
  runOpArgs op args
  err <- gotError
  Data.Bool.bool
    sockGetBool
    (do
      -- TODO: errorExitStatus, head
      Logger_Error{..} <- head <$> getError
      throwError $ Data.ByteString.Char8.unpack errorMessage
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

  soc <- asks storeConfig_socket
  encoder (liftIO . sendAll soc)

  out <- processOutput
  modify (\(a, b) -> (a, b <> out))
  err <- gotError
  Control.Monad.when err $ do
    -- TODO: errorExitStatus, head
    Logger_Error{..} <- head <$> getError
    throwError $ Data.ByteString.Char8.unpack errorMessage

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
  bracket open (S.close . storeConfig_socket) run

 where
  open = do
    soc <- S.socket sockFamily S.Stream 0
    S.connect soc sockAddr
    pure StoreConfig
        { storeConfig_dir = storeRootDir
        , storeConfig_protoVersion = ourProtoVersion
        , storeConfig_socket = soc
        }

  greet = do
    sockPut $ putInt workerMagic1
    soc      <- asks storeConfig_socket
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
