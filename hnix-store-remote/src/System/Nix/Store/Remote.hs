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
  , runStoreConnection
  , runStoreSocket
  -- ** Daemon
  , runDaemon
  , runDaemonConnection
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
import System.Nix.Store.Remote.Server (WorkerHelper, runProxyDaemon)
import System.Nix.Store.Remote.Types

import qualified Control.Monad.Catch
import qualified Network.Socket
-- see TODO bellow
--import qualified System.Directory

-- wip justdoit
import System.Nix.StorePath (StorePath)
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
runStore = runStoreConnection def

runStoreConnection
  :: ( MonadIO m
     , MonadMask m
     )
  => StoreConnection
  -> RemoteStoreT m a
  -> Run m a
runStoreConnection sc k =
  connectionToSocket sc
  >>= \case
    Left e -> pure (Left e, mempty)
    Right (fam, sock) -> runStoreSocket fam sock k

runStoreSocket
  :: ( MonadIO m
     , MonadMask m
     )
  => Family
  -> SockAddr
  -> RemoteStoreT m a
  -> Run m a
runStoreSocket sockFamily sockAddr code =
  Control.Monad.Catch.bracket
    (liftIO open)
    (liftIO . Network.Socket.close . hasStoreSocket)
    (\s -> runRemoteStoreT s $ greetServer >> code)
  where
    open = do
      soc <-
        Network.Socket.socket
          sockFamily
          Network.Socket.Stream
          Network.Socket.defaultProtocol
      Network.Socket.connect soc sockAddr
      pure soc

justdoit :: Run IO (Bool, Bool)
justdoit = do
  runDaemonConnection handler (StoreConnection_Socket "/tmp/dsock") $
    runStoreConnection (StoreConnection_Socket "/tmp/dsock")
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
  runDaemonConnection
    workerHelper
    def

-- | Run an emulated nix daemon using given @StoreConnection@
-- the deamon will close when the continuation returns.
runDaemonConnection
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> StoreConnection
  -> m a
  -> m a
runDaemonConnection workerHelper sc k =
  connectionToSocket sc
  >>= \case
    Left e -> error $ show e
    Right (fam, sock) -> runDaemonSocket workerHelper fam sock k

-- | Run an emulated nix daemon using given @StoreConnection@
-- the deamon will close when the continuation returns.
runDaemonSocket
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> Family
  -> SockAddr
  -> m a
  -> m a
runDaemonSocket workerHelper sockFamily sockAddr k =
  Control.Monad.Catch.bracket
    (liftIO
      $ Network.Socket.socket
          sockFamily
          Network.Socket.Stream
          Network.Socket.defaultProtocol
    )
    (\lsock -> liftIO $ Network.Socket.close lsock) -- *> System.Directory.removeFile f)
    $ \lsock -> do
    --                                                                     ^^^^^^^^^^^^
    -- TODO:  this: -------------------------------------------------------////////////
    -- should really be
    -- a file lock followed by unlink *before* bind rather than after close.  If
    -- the program crashes (or loses power or something) then a stale unix
    -- socket will stick around and prevent the daemon from starting.  using a
    -- lock file instead means only one "copy" of the daemon can hold the lock,
    -- and can safely unlink the socket before binding no matter how shutdown
    -- occured.

    -- set up the listening socket
    liftIO $ Network.Socket.bind lsock sockAddr
    runProxyDaemon workerHelper lsock k

connectionToSocket
  :: MonadIO m
  => StoreConnection
  -> m (Either RemoteStoreError (Family, SockAddr))
connectionToSocket (StoreConnection_Socket (StoreSocketPath f)) =
  pure $ pure
    ( Network.Socket.AF_UNIX
    , SockAddrUnix f
    )
connectionToSocket (StoreConnection_TCP StoreTCP{..}) = do
  addrInfo <- liftIO $ Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just storeTCPHost)
    (Just $ show storeTCPPort)
  case addrInfo of
      (sockAddr:_) ->
        pure $ pure
          ( Network.Socket.addrFamily sockAddr
          , Network.Socket.addrAddress sockAddr
          )
      _ -> pure (Left RemoteStoreError_GetAddrInfoFailed)
