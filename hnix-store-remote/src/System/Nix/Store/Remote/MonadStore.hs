{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Store.Remote.MonadStore
  ( RemoteStoreState(..)
  , RemoteStoreError(..)
  , WorkerError(..)
  , RemoteStoreT
  , runRemoteStoreT
  , mapStoreConfig
  , MonadRemoteStore0
  , MonadRemoteStore
  , MonadRemoteStoreHandshake
  -- * Reader helpers
  , getStoreDir
  , getStoreSocket
  , getProtoVersion
  -- * Logs
  , appendLogs
  , getLogs
  , flushLogs
  , gotError
  , getErrors
  -- * Data required from client
  , getData
  , setData
  , clearData
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State.Strict (get, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (StateT, runStateT, mapStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT, mapExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Network.Socket (Socket)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Serializer (HandshakeSError, LoggerSError, SError)
import System.Nix.Store.Remote.Types.Logger (Logger, isError)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (HasStoreSocket(..), PreStoreConfig, StoreConfig)

data RemoteStoreState = RemoteStoreState {
    remoteStoreState_logs :: [Logger]
  , remoteStoreState_mData :: Maybe ByteString
  } deriving (Eq, Ord, Show)

data RemoteStoreError
  = RemoteStoreError_Fixme String
  | RemoteStoreError_BuildFailed
  | RemoteStoreError_ClientVersionTooOld
  | RemoteStoreError_Disconnected
  | RemoteStoreError_GetAddrInfoFailed
  | RemoteStoreError_SerializerGet SError
  | RemoteStoreError_SerializerHandshake HandshakeSError
  | RemoteStoreError_SerializerLogger LoggerSError
  | RemoteStoreError_SerializerPut SError
  | RemoteStoreError_NoDataProvided
  | RemoteStoreError_ProtocolMismatch
  | RemoteStoreError_WorkerMagic2Mismatch
  | RemoteStoreError_WorkerError WorkerError
  deriving (Eq, Show, Ord)

-- | Non-fatal (to server) errors in worker interaction
data WorkerError
  = WorkerError_SendClosed
  | WorkerError_InvalidOperation Word64
  | WorkerError_NotYetImplemented
  deriving (Eq, Ord, Show)

newtype RemoteStoreT r m a = RemoteStoreT
  { _unRemoteStoreT
      :: ExceptT RemoteStoreError
          (StateT RemoteStoreState
            (ReaderT r m)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    --, MonadState StoreState -- Avoid making the internal state explicit
    , MonadError RemoteStoreError
    , MonadIO
    )

instance MonadTrans (RemoteStoreT r) where
  lift = RemoteStoreT . lift . lift . lift

-- | Runner for @RemoteStoreT@
runRemoteStoreT
  :: ( HasStoreDir r
     , HasStoreSocket r
     , Monad m
     )
  => r
  -> RemoteStoreT r m a
  -> m (Either RemoteStoreError a, [Logger])
runRemoteStoreT r =
    fmap (\(res, RemoteStoreState{..}) -> (res, remoteStoreState_logs))
  . (`runReaderT` r)
  . (`runStateT` emptyState)
  . runExceptT
  . _unRemoteStoreT
  where
    emptyState = RemoteStoreState
      { remoteStoreState_logs = mempty
      , remoteStoreState_mData = Nothing
      }

type MonadRemoteStore0 r = RemoteStoreT r IO

type MonadRemoteStore = MonadRemoteStore0 StoreConfig

type MonadRemoteStoreHandshake = MonadRemoteStore0 PreStoreConfig

mapStoreConfig
  :: (rb -> ra)
  -> (MonadRemoteStore0 ra a -> MonadRemoteStore0 rb a)
mapStoreConfig f =
  RemoteStoreT
  . ( mapExceptT
    . mapStateT
    . withReaderT
    ) f
  . _unRemoteStoreT

-- | Ask for a @StoreDir@
getStoreDir :: HasStoreDir r => MonadRemoteStore0 r StoreDir
getStoreDir = hasStoreDir <$> RemoteStoreT ask

-- | Ask for a @StoreDir@
getStoreSocket :: HasStoreSocket r => MonadRemoteStore0 r Socket
getStoreSocket = hasStoreSocket <$> RemoteStoreT ask

-- | Ask for a @StoreDir@
getProtoVersion :: HasProtoVersion r => MonadRemoteStore0 r ProtoVersion
getProtoVersion = hasProtoVersion <$> RemoteStoreT ask

-- * Logs

gotError :: MonadRemoteStore0 r Bool
gotError = any isError <$> getLogs

getErrors :: MonadRemoteStore0 r [Logger]
getErrors = filter isError <$> getLogs

appendLogs :: [Logger] -> MonadRemoteStore0 r ()
appendLogs x = RemoteStoreT
  $ modify
  $ \s -> s { remoteStoreState_logs = remoteStoreState_logs s <> x }

getLogs :: MonadRemoteStore0 r [Logger]
getLogs = remoteStoreState_logs <$> RemoteStoreT get

flushLogs :: MonadRemoteStore0 r ()
flushLogs = RemoteStoreT $ modify $ \s -> s { remoteStoreState_logs = mempty }

-- * Data required from client

getData :: MonadRemoteStore0 r (Maybe ByteString)
getData = remoteStoreState_mData <$> RemoteStoreT get

setData :: ByteString -> MonadRemoteStore0 r ()
setData x = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mData = pure x }

clearData :: MonadRemoteStore0 r ()
clearData = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mData = Nothing }
