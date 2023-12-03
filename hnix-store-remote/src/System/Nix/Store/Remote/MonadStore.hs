{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Store.Remote.MonadStore
  ( RemoteStoreState(..)
  , RemoteStoreError(..)
  , WorkerError(..)
  , WorkerException(..)
  , RemoteStoreT
  , runRemoteStoreT
  , mapStoreConfig
  , MonadRemoteStoreR(..)
  , MonadRemoteStore
  , getProtoVersion
  ) where

import Control.Exception (SomeException)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.State.Strict (get, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (StateT, runStateT, mapStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT, mapExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Network.Socket (Socket)
import System.Nix.Nar (NarSource)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Serializer (HandshakeSError, LoggerSError, SError)
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (HasStoreSocket(..), StoreConfig)

data RemoteStoreState = RemoteStoreState {
    remoteStoreState_logs :: [Logger]
  , remoteStoreState_gotError :: Bool
  , remoteStoreState_mDataSource :: Maybe (Word64 -> IO (Maybe ByteString))
  -- ^ Source for @Logger_Read@, this will be called repeatedly
  -- as the daemon requests chunks of size @Word64@.
  -- If the function returns Nothing and daemon tries to read more
  -- data an error is thrown.
  , remoteStoreState_mNarSource :: Maybe (NarSource IO)
  }

data RemoteStoreError
  = RemoteStoreError_Fixme String
  | RemoteStoreError_BuildFailed
  | RemoteStoreError_ClientVersionTooOld
  | RemoteStoreError_Disconnected
  | RemoteStoreError_GetAddrInfoFailed
  | RemoteStoreError_GenericIncrementalLeftovers String ByteString -- when there are bytes left over after genericIncremental parser is done, (Done x leftover), first param is show x
  | RemoteStoreError_GenericIncrementalFail String ByteString -- when genericIncremental parser returns ((Fail msg leftover) :: Result)
  | RemoteStoreError_SerializerGet SError
  | RemoteStoreError_SerializerHandshake HandshakeSError
  | RemoteStoreError_SerializerLogger LoggerSError
  | RemoteStoreError_SerializerPut SError
  | RemoteStoreError_IOException SomeException
  | RemoteStoreError_LoggerLeftovers String ByteString -- when there are bytes left over after incremental logger parser is done, (Done x leftover), first param is show x
  | RemoteStoreError_LoggerParserFail String ByteString -- when incremental parser returns ((Fail msg leftover) :: Result)
  | RemoteStoreError_NoDataSourceProvided -- remoteStoreState_mDataSource is required but it is Nothing
  | RemoteStoreError_DataSourceExhausted -- remoteStoreState_mDataSource returned Nothing but more data was requested
  | RemoteStoreError_NoNarSourceProvided
  | RemoteStoreError_OperationFailed
  | RemoteStoreError_ProtocolMismatch
  | RemoteStoreError_RapairNotSupportedByRemoteStore -- "repairing is not supported when building through the Nix daemon"
  | RemoteStoreError_WorkerMagic2Mismatch
  | RemoteStoreError_WorkerError WorkerError
  -- bad / redundant
  | RemoteStoreError_WorkerException WorkerException
  deriving Show

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
      , remoteStoreState_gotError = False
      , remoteStoreState_mDataSource = Nothing
      , remoteStoreState_mNarSource = Nothing
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

class ( MonadIO m
      , MonadError RemoteStoreError m
      , HasStoreSocket r
      , HasStoreDir r
      , MonadReader r m
      )
      => MonadRemoteStoreR r m where

  appendLog :: Logger -> m ()
  default appendLog
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => Logger
    -> m ()
  appendLog = lift . appendLog

  setError :: m ()
  default setError
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => m ()
  setError = lift setError

  clearError :: m ()
  default clearError
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => m ()
  clearError = lift clearError

  gotError :: m Bool
  default gotError
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => m Bool
  gotError = lift gotError

  getStoreDir :: m StoreDir
  default getStoreDir
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => m StoreDir
  getStoreDir = lift getStoreDir

  getStoreSocket :: m Socket
  default getStoreSocket
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => m Socket
  getStoreSocket = lift getStoreSocket

  setNarSource :: NarSource IO -> m ()
  default setNarSource
   :: ( MonadTrans t
      , MonadRemoteStoreR r m'
      , m ~ t m'
      )
   => NarSource IO
   -> m ()
  setNarSource x = lift (setNarSource x)

  takeNarSource :: m (Maybe (NarSource IO))
  default takeNarSource
   :: ( MonadTrans t
      , MonadRemoteStoreR r m'
      , m ~ t m'
      )
   => m (Maybe (NarSource IO))
  takeNarSource = lift takeNarSource

  setDataSource :: (Word64 -> IO (Maybe ByteString)) -> m ()
  default setDataSource
   :: ( MonadTrans t
      , MonadRemoteStoreR r m'
      , m ~ t m'
      )
   => (Word64 -> IO (Maybe ByteString))
   -> m ()
  setDataSource x = lift (setDataSource x)

  getDataSource :: m (Maybe (Word64 -> IO (Maybe ByteString)))
  default getDataSource
   :: ( MonadTrans t
      , MonadRemoteStoreR r m'
      , m ~ t m'
      )
   => m (Maybe (Word64 -> IO (Maybe ByteString)))
  getDataSource = lift getDataSource

  clearDataSource :: m ()
  default clearDataSource
    :: ( MonadTrans t
       , MonadRemoteStoreR r m'
       , m ~ t m'
       )
    => m ()
  clearDataSource = lift clearDataSource



instance MonadRemoteStoreR r m => MonadRemoteStoreR r (StateT s m)
instance MonadRemoteStoreR r m => MonadRemoteStoreR r (ReaderT r m)
instance MonadRemoteStoreR r m => MonadRemoteStoreR r (ExceptT RemoteStoreError m)

type MonadRemoteStore m = MonadRemoteStoreR StoreConfig m

instance ( MonadIO m
         , HasStoreDir r
         , HasStoreSocket r
         )
         => MonadRemoteStoreR r (RemoteStoreT r m) where

  getStoreDir = hasStoreDir <$> RemoteStoreT ask
  getStoreSocket = hasStoreSocket <$> RemoteStoreT ask

  appendLog x =
    RemoteStoreT
    $ modify
    $ \s -> s { remoteStoreState_logs = remoteStoreState_logs s ++ [x] }

  setError = RemoteStoreT $ modify $ \s -> s { remoteStoreState_gotError = True }
  clearError = RemoteStoreT $ modify $ \s -> s { remoteStoreState_gotError = False }
  gotError = remoteStoreState_gotError <$> RemoteStoreT get

  setDataSource x = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mDataSource = pure x }
  getDataSource = remoteStoreState_mDataSource <$> RemoteStoreT get
  clearDataSource = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mDataSource = Nothing }

  setNarSource x = RemoteStoreT $ modify $ \s -> s { remoteStoreState_mNarSource = pure x }
  takeNarSource = RemoteStoreT $ do
    x <- remoteStoreState_mNarSource <$> get
    modify $ \s -> s { remoteStoreState_mNarSource = Nothing }
    pure x

-- | Ask for a @StoreDir@
getProtoVersion
  :: ( MonadRemoteStoreR r m
     , HasProtoVersion r
     )
  => m ProtoVersion
getProtoVersion = asks hasProtoVersion
