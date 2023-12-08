{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module System.Nix.Store.Remote
  ( module System.Nix.Store.Types
  , module System.Nix.Store.Remote.Client
  , module System.Nix.Store.Remote.MonadStore
  , module System.Nix.Store.Remote.Types
  -- * Compat
  , MonadStore
  -- * Runners
  , runStore
  , runStoreOpts
  , runStoreOptsTCP
  -- ** Daemon
  , runDaemon
  , runDaemonOpts
  , justdoit
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

-- wip daemon
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Nix.StorePath (StorePath)
import System.Nix.Store.Remote.Server (WorkerHelper, runDaemonSocket)
import qualified System.Directory
import qualified System.Nix.StorePath
import qualified Control.Monad.Catch

-- * Compat

type MonadStore = RemoteStoreT StoreConfig IO

-- * Runners

runStore :: MonadStore a -> Run IO a
runStore = runStoreOpts defaultSockPath def

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

justdoit :: Run IO (Bool, Bool)
justdoit = do
  runDaemonOpts def handler "/tmp/dsock" $
    runStoreOpts "/tmp/dsock" def
      $ do
        a <- isValidPath pth
        b <- isValidPath pth
        pure (a, b)
  where
    pth :: StorePath
    pth =
      either (error . show) id
      $ System.Nix.StorePath.parsePathFromText
        def
        "/nix/store/yyznqbwam67cmp7zfwk0rkgmi9yqsdsm-hnix-store-core-0.8.0.0"

    handler :: MonadIO m => WorkerHelper m
    handler k = do
      x <- liftIO $ runStore $ doReq k
      either (error . show) pure (fst x)

runDaemon
  :: forall m a
  . ( MonadIO m
    , MonadBaseControl IO m
    , MonadConc m
    )
  => WorkerHelper m
  -> m a
  -> m a
runDaemon workerHelper k = runDaemonOpts def workerHelper defaultSockPath k

-- | Run an emulated nix daemon on given socket address.
-- the deamon will close when the continuation returns.
runDaemonOpts
  :: forall m a
  . ( MonadIO m
    , MonadBaseControl IO m
    , MonadConc m
    )
  => StoreDir
  -> WorkerHelper m
  -> FilePath
  -> m a
  -> m a
runDaemonOpts sd workerHelper f k = Control.Monad.Catch.bracket
  (liftIO
    $ Network.Socket.socket
        Network.Socket.AF_UNIX
        Network.Socket.Stream
        Network.Socket.defaultProtocol
  )
  (\lsock -> liftIO $ Network.Socket.close lsock *> System.Directory.removeFile f)
  $ \lsock -> do
  --                                                                 ^^^^^^^^^^^^
  -- TODO:  this: --------------------------------------------------////////////
  -- should really be
  -- a file lock followed by unlink *before* bind rather than after close.  If
  -- the program crashes (or loses power or something) then a stale unix
  -- socket will stick around and prevent the daemon from starting.  using a
  -- lock file instead means only one "copy" of the daemon can hold the lock,
  -- and can safely unlink the socket before binding no matter how shutdown
  -- occured.

  -- set up the listening socket
  liftIO $ Network.Socket.bind lsock (SockAddrUnix f)
  runDaemonSocket sd workerHelper lsock k
