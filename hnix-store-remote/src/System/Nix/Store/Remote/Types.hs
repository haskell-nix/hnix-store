{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Store.Remote.Types (
    MonadStore
  , MonadStoreT(..)
  , StoreConfig(..)
  , Logger(..)
  , Field(..)
  , getStoreDir
  , getLog
  , flushLog
  , gotError
  , getError
  , setData
  , clearData
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

newtype MonadStoreT m a = NixStore {
    unStore :: ExceptT String (StateT (Maybe BSL.ByteString, [Logger]) (ReaderT StoreConfig m)) a
  } deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadError String
    , MonadIO
    )

instance MonadTrans MonadStoreT where
  lift = NixStore . lift . lift . lift

type MonadStore a = MonadStoreT IO a

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

gotError :: (MonadIO m) => MonadStoreT m Bool
gotError = any isError . snd <$> NixStore get

getError :: (MonadIO m) => MonadStoreT m [Logger]
getError = filter isError . snd <$> NixStore get

getLog :: (MonadIO m) => MonadStoreT m [Logger]
getLog = snd <$> NixStore get

flushLog :: (MonadIO m) => MonadStoreT m ()
flushLog = NixStore $ modify (\(a, _b) -> (a, []))

setData :: (MonadIO m) => BSL.ByteString -> MonadStoreT m ()
setData x = NixStore $ modify (\(_, b) -> (Just x, b))

clearData :: (MonadIO m) => MonadStoreT m ()
clearData = NixStore $ modify (\(_, b) -> (Nothing, b))

getStoreDir :: (MonadIO m) => MonadStoreT m FilePath
getStoreDir = storeDir <$> NixStore ask
