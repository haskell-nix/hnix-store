module System.Nix.Store.Remote
  (
    module System.Nix.Store.Types
  , module System.Nix.Store.Remote.Client
  , module System.Nix.Store.Remote.MonadStore
  , module System.Nix.Store.Remote.Types
  -- * Compat
  , MonadStore
  -- * Runners
  , runStore
  , runStoreOpts
  , runStoreOptsTCP
  ) where

import Data.Default.Class (Default(def))
import Network.Socket (Family, SockAddr(SockAddrUnix))
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))
import System.Nix.StorePath (StoreDir)
import System.Nix.Store.Remote.MonadStore (RemoteStoreT, getStoreDir, RemoteStoreError(RemoteStoreError_GetAddrInfoFailed))
import System.Nix.Store.Remote.Client
import System.Nix.Store.Remote.Types

import qualified Control.Exception
import qualified Network.Socket

-- * Compat

type MonadStore = RemoteStoreT StoreConfig IO

-- * Runners

runStore :: MonadStore a -> Run IO a
runStore = runStoreOpts defaultSockPath def
  where
    defaultSockPath :: String
    defaultSockPath = "/nix/var/nix/daemon-socket/socket"

runStoreOpts
  :: FilePath
  -> StoreDir
  -> MonadStore a
  -> Run IO a
runStoreOpts socketPath =
  runStoreOpts'
    Network.Socket.AF_UNIX
    (SockAddrUnix socketPath)

runStoreOptsTCP
  :: String
  -> Int
  -> StoreDir
  -> MonadStore a
  -> Run IO a
runStoreOptsTCP host port sd code = do
  Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just host)
    (Just $ show port)
    >>= \case
      (sockAddr:_) ->
        runStoreOpts'
          (Network.Socket.addrFamily sockAddr)
          (Network.Socket.addrAddress sockAddr)
          sd
          code
      _ -> pure (Left RemoteStoreError_GetAddrInfoFailed, mempty)

runStoreOpts'
  :: Family
  -> SockAddr
  -> StoreDir
  -> MonadStore a
  -> Run IO a
runStoreOpts' sockFamily sockAddr storeRootDir code =
  Control.Exception.bracket
    open
    (Network.Socket.close . hasStoreSocket)
    (flip runStoreSocket code)
  where
    open = do
      soc <- Network.Socket.socket sockFamily Network.Socket.Stream 0
      Network.Socket.connect soc sockAddr
      pure PreStoreConfig
          { preStoreConfig_socket = soc
          , preStoreConfig_dir = storeRootDir
          }
