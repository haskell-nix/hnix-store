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

import Control.Monad.Catch (MonadMask)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class (Default(def))
import Network.Socket (Family, SockAddr(SockAddrUnix))
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))
import System.Nix.Store.Remote.MonadStore
  ( runRemoteStoreT
  , MonadRemoteStore(..)
  , RemoteStoreT
  , RemoteStoreError(RemoteStoreError_GetAddrInfoFailed))
import System.Nix.Store.Remote.Client
import System.Nix.Store.Remote.Types

import qualified Control.Monad.Catch
import qualified Network.Socket
import qualified System.Directory

-- wip daemon
import System.Nix.StorePath (StorePath)
import System.Nix.Store.Remote.Server (WorkerHelper, runDaemonSocket)
import qualified System.Nix.StorePath

-- * Compat

type MonadStore = RemoteStoreT IO

-- * Runners

runStore
  :: ( MonadIO m
     , MonadMask m
     )
  => RemoteStoreT m a
  -> Run m a
runStore = runStoreOpts defaultSockPath

defaultSockPath :: String
defaultSockPath = "/nix/var/nix/daemon-socket/socket"

runStoreOpts
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> RemoteStoreT m a
  -> Run m a
runStoreOpts socketPath =
  runStoreOpts'
    Network.Socket.AF_UNIX
    (SockAddrUnix socketPath)

runStoreOptsTCP
  :: ( MonadIO m
     , MonadMask m
     )
  => String
  -> Int
  -> RemoteStoreT m a
  -> Run m a
runStoreOptsTCP host port code = do
  addrInfo <- liftIO $ Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just host)
    (Just $ show port)
  case addrInfo of
      (sockAddr:_) ->
        runStoreOpts'
          (Network.Socket.addrFamily sockAddr)
          (Network.Socket.addrAddress sockAddr)
          code
      _ -> pure (Left RemoteStoreError_GetAddrInfoFailed, mempty)

runStoreOpts'
  :: ( MonadIO m
     , MonadMask m
     )
  => Family
  -> SockAddr
  -> RemoteStoreT m a
  -> Run m a
runStoreOpts' sockFamily sockAddr code =
  Control.Monad.Catch.bracket
    (liftIO open)
    (liftIO . Network.Socket.close . hasStoreSocket)
    (\s -> runRemoteStoreT s $ runStoreSocket code)
  where
    open = do
      soc <- Network.Socket.socket sockFamily Network.Socket.Stream 0
      Network.Socket.connect soc sockAddr
      pure soc

justdoit :: Run IO (Bool, Bool)
justdoit = do
  runDaemonOpts handler "/tmp/dsock" $
    runStoreOpts "/tmp/dsock"
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
    , MonadConc m
    )
  => WorkerHelper m
  -> m a
  -> m a
runDaemon workerHelper =
  runDaemonOpts
    workerHelper
    defaultSockPath

-- | Run an emulated nix daemon on given socket address.
-- the deamon will close when the continuation returns.
runDaemonOpts
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> FilePath
  -> m a
  -> m a
runDaemonOpts  workerHelper f k = Control.Monad.Catch.bracket
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
  liftIO $ Network.Socket.bind lsock (SockAddrUnix f)
  runDaemonSocket workerHelper lsock k
