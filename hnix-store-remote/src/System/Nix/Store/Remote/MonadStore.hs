{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Store.Remote.MonadStore
  ( RemoteStoreState(..)
  , RemoteStoreError(..)
  , WorkerError(..)
  , WorkerException(..)
  , RemoteStoreT
  , runRemoteStoreT
  , mapStoreConfig
  , MonadRemoteStore(..)
  , getProtoVersion
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
import System.Nix.Store.Remote.Types.StoreConfig (HasStoreSocket(..))

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
  -- bad / redundant
  | RemoteStoreError_WorkerException WorkerException
  deriving (Eq, Show, Ord)

-- | fatal error in worker interaction which should disconnect client.
data WorkerException
  = WorkerException_ClientVersionTooOld
  | WorkerException_ProtocolMismatch
  | WorkerException_Error WorkerError
  -- ^ allowed error outside allowed worker state
--  | WorkerException_DecodingError DecodingError
--  | WorkerException_BuildFailed StorePath
  deriving (Eq, Ord, Show)

-- | Non-fatal (to server) errors in worker interaction
data WorkerError
  = WorkerError_SendClosed
  | WorkerError_InvalidOperation Word64
  | WorkerError_NotYetImplemented
  | WorkerError_UnsupportedOperation
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

mapStoreConfig
  :: (rb -> ra)
  -> (RemoteStoreT ra m a -> RemoteStoreT rb m a)
mapStoreConfig f =
  RemoteStoreT
  . ( mapExceptT
    . mapStateT
    . withReaderT
    ) f
  . _unRemoteStoreT

class ( Monad m
      , MonadError RemoteStoreError m
      )
      => MonadRemoteStore m where

  appendLogs :: [Logger] -> m ()
  default appendLogs
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => [Logger]
    -> m ()
  appendLogs = lift . appendLogs

  gotError :: m Bool
  default gotError
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m Bool
  gotError = lift gotError

  getErrors :: m [Logger]
  default getErrors
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m [Logger]
  getErrors = lift getErrors

  getLogs :: m [Logger]
  default getLogs
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m [Logger]
  getLogs = lift getLogs

  flushLogs :: m ()
  default flushLogs
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ()
  flushLogs = lift flushLogs

  setData :: ByteString -> m ()
  default setData
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => ByteString
   -> m ()
  setData = lift . setData

  getData :: m (Maybe ByteString)
  default getData
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m (Maybe ByteString)
  getData = lift getData

  clearData :: m ()
  default clearData
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ()
  clearData = lift clearData

  getStoreDir :: m StoreDir
  default getStoreDir
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m StoreDir
  getStoreDir = lift getStoreDir

  getStoreSocket :: m Socket
  default getStoreSocket
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m Socket
  getStoreSocket = lift getStoreSocket

instance MonadRemoteStore m => MonadRemoteStore (StateT s m)
instance MonadRemoteStore m => MonadRemoteStore (ReaderT r m)
instance MonadRemoteStore m => MonadRemoteStore (ExceptT RemoteStoreError m)

instance ( Monad m
         , HasStoreDir r
         , HasStoreSocket r
         )
         => MonadRemoteStore (RemoteStoreT r m) where

  getStoreDir = hasStoreDir <$> RemoteStoreT ask
  getStoreSocket = hasStoreSocket <$> RemoteStoreT ask

  appendLogs x =
    RemoteStoreT
    $ modify
    $ \s -> s { remoteStoreState_logs = remoteStoreState_logs s <> x }
  getLogs = remoteStoreState_logs <$> RemoteStoreT get
  flushLogs =
    RemoteStoreT
    $ modify
    $ \s -> s { remoteStoreState_logs = mempty }
  gotError = any isError <$> getLogs
  getErrors = filter isError <$> getLogs

  getData = remoteStoreState_mData <$> RemoteStoreT get
  setData x = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mData = pure x }
  clearData = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mData = Nothing }

-- | Ask for a @StoreDir@
getProtoVersion
  :: ( Monad m
     , HasProtoVersion r
     )
  => RemoteStoreT r m ProtoVersion
getProtoVersion = hasProtoVersion <$> RemoteStoreT ask
