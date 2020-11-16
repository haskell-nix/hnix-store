{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- XXX (layus 2020-11) Are all of these needed ?
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Nix.Store.Remote.Types (
    RemoteStoreT(..)
  , MonadRemoteStore(..)
  , StoreConfig(..)
  , StoreState(..)
  , Logger(..)
  , Field(..)
  ) where


import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as BSL
import           Network.Socket            (Socket)
import           Control.Applicative       (Alternative)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Fail             ( MonadFail )

data StoreConfig = StoreConfig {
    storeDir        :: FilePath
  , storeSocket     :: Socket
  }

data StoreState = StoreState {
    logs            :: [Logger]
  , mData            :: Maybe BSL.ByteString
  }

newtype RemoteStoreT m a = RemoteStore {
    unStore :: ExceptT String (StateT StoreState (ReaderT StoreConfig m)) a
  } deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    --, MonadReader StoreConfig -- Avoid making the internal state explicit
    --, MonadState StoreState   -- Avoid making the internal state explicit
    , MonadFail
    , MonadError String
    , MonadIO
    )

instance MonadTrans RemoteStoreT where
  lift = RemoteStore . lift . lift . lift


type ActivityID = Int
type ActivityParentID = Int
type ActivityType = Int
type Verbosity = Int
type ResultType = Int

data Field = LogStr ByteString | LogInt Int
  deriving (Eq, Ord, Show)

data Logger =
    Next          ByteString
  | Read          Int            -- data needed from source
  | Write         ByteString -- data for sink
  | Last
  | Error         Int ByteString
  | StartActivity ActivityID Verbosity ActivityType ByteString [Field] ActivityParentID
  | StopActivity  ActivityID
  | Result        ActivityID ResultType [Field]
  deriving (Eq, Ord, Show)

isError :: Logger -> Bool
isError (Error _ _) = True
isError _           = False

class (Monad m, MonadError String m) => MonadRemoteStore m where
  gotError :: m Bool
  default gotError :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m Bool
  gotError = lift gotError

  getError :: m [Logger]
  default getError :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m [Logger]
  getError = lift getError

  getLog :: m [Logger]
  default getLog :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m [Logger]
  getLog = lift getLog

  setLog :: [Logger] -> m ()
  default setLog :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => [Logger] -> m ()
  setLog = lift . setLog

  flushLog :: m ()
  default flushLog  :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m ()
  flushLog = lift flushLog

  setData :: BSL.ByteString -> m ()
  default setData :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => BSL.ByteString -> m ()
  setData = lift . setData

  getData :: m (Maybe BSL.ByteString)
  default getData :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m (Maybe BSL.ByteString)
  getData = lift getData

  clearData :: m ()
  default clearData :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m ()
  clearData = lift clearData

  getStoreDir :: m FilePath
  default getStoreDir  :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m FilePath
  getStoreDir = lift getStoreDir

  getSocket :: m Socket
  default getSocket :: (MonadTrans t, MonadRemoteStore m', m ~ t m') => m Socket
  getSocket = lift getSocket

instance (MonadRemoteStore m) => MonadRemoteStore (StateT s m)
instance (MonadRemoteStore m) => MonadRemoteStore (ReaderT r m)
-- instance (MonadRemoteStore m) => MonadError String m

instance (Monad m) => MonadRemoteStore (RemoteStoreT m) where
  gotError = any isError . logs <$> RemoteStore get
  
  getError = filter isError . logs <$> RemoteStore get
  
  getLog = logs <$> RemoteStore get
  
  flushLog = RemoteStore $ modify (\s -> s { logs = [] })

  setLog logs = RemoteStore $ modify (\s -> s { logs = logs })
  
  setData x = RemoteStore $ modify (\s -> s { mData = Just x })
  
  clearData = RemoteStore $ modify (\s -> s { mData = Nothing })

  getData = RemoteStore $ gets mData
  
  getStoreDir = storeDir <$> RemoteStore ask

  getSocket = storeSocket <$> RemoteStore ask

