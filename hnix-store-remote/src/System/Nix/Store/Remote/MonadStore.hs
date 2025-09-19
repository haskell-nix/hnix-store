{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Store.Remote.MonadStore
  ( RemoteStoreState(..)
  , RemoteStoreError(..)
  , WorkerError(..)
  , WorkerException(..)
  , RemoteStoreT
  , runRemoteStoreT
  , MonadRemoteStore(..)
  ) where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State.Strict (get, gets, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Data.Default.Class (Default(def))
import Data.DList (DList)
import Data.Word (Word64)
import Network.Socket (Socket)
import System.Nix.Nar (NarSource)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Serializer (HandshakeSError, LoggerSError, RequestSError, ReplySError, SError)
import System.Nix.Store.Remote.Types.Logger (Logger, BasicError, ErrorInfo)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (ProtoStoreConfig(..))

import Data.DList qualified

data RemoteStoreState = RemoteStoreState {
    remoteStoreStateConfig :: ProtoStoreConfig
  , remoteStoreStateLogs :: DList Logger
  , remoteStoreStateMDataSource :: Maybe (Word64 -> IO (Maybe ByteString))
  -- ^ Source for @Logger_Read@, this will be called repeatedly
  -- as the daemon requests chunks of size @Word64@.
  -- If the function returns Nothing and daemon tries to read more
  -- data an error is thrown.
  -- Used by @AddToStoreNar@ and @ImportPaths@ operations.
  , remoteStoreStateMDataSink :: Maybe (ByteString -> IO ())
  -- ^ Sink for @Logger_Write@, called repeatedly by the daemon
  -- to dump us some data. Used by @ExportPath@ operation.
  , remoteStoreStateMDataSinkSize :: Maybe Word64
  -- ^ Byte length to be written to the sink, for NarForPath
  , remoteStoreStateMNarSource :: Maybe (NarSource IO)
  }

instance HasStoreDir RemoteStoreState where
  hasStoreDir = hasStoreDir . remoteStoreStateConfig

instance HasProtoVersion RemoteStoreState where
  hasProtoVersion = hasProtoVersion . remoteStoreStateConfig

data RemoteStoreError
  = RemoteStoreError_Fixme String
  | RemoteStoreError_BuildFailed
  | RemoteStoreError_ClientVersionTooOld
  | RemoteStoreError_DerivationParse String
  | RemoteStoreError_Disconnected
  | RemoteStoreError_GetAddrInfoFailed
  | RemoteStoreError_GenericIncrementalLeftovers String ByteString -- when there are bytes left over after genericIncremental parser is done, (Done x leftover), first param is show x
  | RemoteStoreError_GenericIncrementalFail String ByteString -- when genericIncremental parser returns ((Fail msg leftover) :: Result)
  | RemoteStoreError_SerializerGet SError
  | RemoteStoreError_SerializerHandshake HandshakeSError
  | RemoteStoreError_SerializerLogger LoggerSError
  | RemoteStoreError_SerializerPut SError
  | RemoteStoreError_SerializerRequest RequestSError
  | RemoteStoreError_SerializerReply ReplySError
  | RemoteStoreError_IOException SomeException
  | RemoteStoreError_LoggerError (Either BasicError ErrorInfo)
  | RemoteStoreError_LoggerLeftovers String ByteString -- when there are bytes left over after incremental logger parser is done, (Done x leftover), first param is show x
  | RemoteStoreError_LoggerParserFail String ByteString -- when incremental parser returns ((Fail msg leftover) :: Result)
  | RemoteStoreError_NoDataSourceProvided -- remoteStoreStateMDataSource is required but it is Nothing
  | RemoteStoreError_DataSourceExhausted -- remoteStoreStateMDataSource returned Nothing but more data was requested
  | RemoteStoreError_DataSourceZeroLengthRead -- remoteStoreStateMDataSource returned a zero length ByteString
  | RemoteStoreError_DataSourceReadTooLarge -- remoteStoreStateMDataSource returned a ByteString larger than the chunk size requested or the remaining bytes
  | RemoteStoreError_NoDataSinkProvided -- remoteStoreStateMDataSink is required but it is Nothing
  | RemoteStoreError_NoDataSinkSizeProvided -- remoteStoreStateMDataSinkSize is required but it is Nothing
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

newtype RemoteStoreT m a = RemoteStoreT
  { _unRemoteStoreT
      :: ExceptT RemoteStoreError
          (StateT RemoteStoreState
            (ReaderT Socket m)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Socket
    --, MonadState StoreState -- Avoid making the internal state explicit
    , MonadError RemoteStoreError
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadIO
    )

instance MonadTrans RemoteStoreT where
  lift = RemoteStoreT . lift . lift . lift

-- | Runner for @RemoteStoreT@
runRemoteStoreT
  :: Monad m
  => Socket
  -> RemoteStoreT m a
  -> m (Either RemoteStoreError a, DList Logger)
runRemoteStoreT sock =
    fmap (\(res, RemoteStoreState{..}) -> (res, remoteStoreStateLogs))
  . (`runReaderT` sock)
  . (`runStateT` emptyState)
  . runExceptT
  . _unRemoteStoreT
  where
    emptyState = RemoteStoreState
      { remoteStoreStateConfig = def
      , remoteStoreStateLogs = mempty
      , remoteStoreStateMDataSource = Nothing
      , remoteStoreStateMDataSink = Nothing
      , remoteStoreStateMDataSinkSize = Nothing
      , remoteStoreStateMNarSource = Nothing
      }

class ( MonadIO m
      , MonadError RemoteStoreError m
      )
      => MonadRemoteStore m where

  appendLog :: Logger -> m ()
  default appendLog
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => Logger
    -> m ()
  appendLog = lift . appendLog

  getConfig :: m ProtoStoreConfig
  default getConfig
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ProtoStoreConfig
  getConfig = lift getConfig

  getStoreDir :: m StoreDir
  default getStoreDir
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m StoreDir
  getStoreDir = lift getStoreDir

  setStoreDir :: StoreDir -> m ()
  default setStoreDir
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => StoreDir
    -> m ()
  setStoreDir = lift . setStoreDir

  -- | Get @ProtoVersion@ from state
  getProtoVersion :: m ProtoVersion
  default getProtoVersion
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ProtoVersion
  getProtoVersion = lift getProtoVersion

  setProtoVersion :: ProtoVersion -> m ()
  default setProtoVersion
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => ProtoVersion
    -> m ()
  setProtoVersion = lift . setProtoVersion

  getStoreSocket :: m Socket
  default getStoreSocket
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m Socket
  getStoreSocket = lift getStoreSocket

  setNarSource :: NarSource IO -> m ()
  default setNarSource
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => NarSource IO
   -> m ()
  setNarSource x = lift (setNarSource x)

  takeNarSource :: m (Maybe (NarSource IO))
  default takeNarSource
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => m (Maybe (NarSource IO))
  takeNarSource = lift takeNarSource

  setDataSource :: (Word64 -> IO (Maybe ByteString)) -> m ()
  default setDataSource
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => (Word64 -> IO (Maybe ByteString))
   -> m ()
  setDataSource x = lift (setDataSource x)

  takeDataSource :: m (Maybe (Word64 -> IO (Maybe ByteString)))
  default takeDataSource
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => m (Maybe (Word64 -> IO (Maybe ByteString)))
  takeDataSource = lift takeDataSource

  getDataSource :: m (Maybe (Word64 -> IO (Maybe ByteString)))
  default getDataSource
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => m (Maybe (Word64 -> IO (Maybe ByteString)))
  getDataSource = lift getDataSource

  clearDataSource :: m ()
  default clearDataSource
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ()
  clearDataSource = lift clearDataSource

  setDataSink :: (ByteString -> IO ()) -> m ()
  default setDataSink
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => (ByteString -> IO ())
   -> m ()
  setDataSink x = lift (setDataSink x)

  getDataSink :: m (Maybe (ByteString -> IO ()))
  default getDataSink
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => m (Maybe (ByteString -> IO ()))
  getDataSink = lift getDataSink

  clearDataSink :: m ()
  default clearDataSink
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ()
  clearDataSink = lift clearDataSink

  setDataSinkSize :: Word64 -> m ()
  default setDataSinkSize
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => Word64
   -> m ()
  setDataSinkSize x = lift (setDataSinkSize x)

  getDataSinkSize :: m (Maybe Word64)
  default getDataSinkSize
   :: ( MonadTrans t
      , MonadRemoteStore m'
      , m ~ t m'
      )
   => m (Maybe Word64)
  getDataSinkSize = lift getDataSinkSize

  clearDataSinkSize :: m ()
  default clearDataSinkSize
    :: ( MonadTrans t
       , MonadRemoteStore m'
       , m ~ t m'
       )
    => m ()
  clearDataSinkSize = lift clearDataSinkSize

instance MonadRemoteStore m => MonadRemoteStore (StateT s m)
instance MonadRemoteStore m => MonadRemoteStore (ReaderT r m)
instance MonadRemoteStore m => MonadRemoteStore (ExceptT RemoteStoreError m)

instance MonadIO m => MonadRemoteStore (RemoteStoreT m) where

  getConfig = RemoteStoreT $ gets remoteStoreStateConfig
  getProtoVersion = RemoteStoreT $ gets hasProtoVersion
  setProtoVersion pv =
    RemoteStoreT $ modify $ \s ->
      s { remoteStoreStateConfig =
            (remoteStoreStateConfig s) { protoStoreConfigProtoVersion = pv }
      }
  getStoreDir = RemoteStoreT $ gets hasStoreDir
  setStoreDir sd =
    RemoteStoreT $ modify $ \s ->
      s { remoteStoreStateConfig =
            (remoteStoreStateConfig s) { protoStoreConfigDir = sd }
      }

  getStoreSocket = RemoteStoreT ask

  appendLog x =
    RemoteStoreT
    $ modify
    $ \s -> s { remoteStoreStateLogs = remoteStoreStateLogs s `Data.DList.snoc` x }

  setDataSource x = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMDataSource = pure x }
  getDataSource = RemoteStoreT (gets remoteStoreStateMDataSource)
  clearDataSource = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMDataSource = Nothing }

  takeDataSource = RemoteStoreT $ do
    x <- remoteStoreStateMDataSource <$> get
    modify $ \s -> s { remoteStoreStateMDataSource = Nothing }
    pure x

  setDataSink x = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMDataSink = pure x }
  getDataSink = RemoteStoreT (gets remoteStoreStateMDataSink)
  clearDataSink = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMDataSink = Nothing }

  setDataSinkSize x = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMDataSinkSize = pure x }
  getDataSinkSize = RemoteStoreT (gets remoteStoreStateMDataSinkSize)
  clearDataSinkSize = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMDataSinkSize = Nothing }

  setNarSource x = RemoteStoreT $ modify $ \s -> s { remoteStoreStateMNarSource = pure x }
  takeNarSource = RemoteStoreT $ do
    x <- remoteStoreStateMNarSource <$> get
    modify $ \s -> s { remoteStoreStateMNarSource = Nothing }
    pure x
