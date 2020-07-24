{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.Store.Remote.Types (
    MonadStore
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
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

data StoreConfig = StoreConfig {
    storeDir        :: FilePath
  , storeSocket     :: Socket
  }

type MonadStore a = ExceptT String (StateT (Maybe BSL.ByteString, [Logger]) (ReaderT StoreConfig IO)) a

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

gotError :: MonadStore Bool
gotError = any isError . snd <$> get

getError :: MonadStore [Logger]
getError = filter isError . snd <$> get

getLog :: MonadStore [Logger]
getLog = snd <$> get

flushLog :: MonadStore ()
flushLog = modify (\(a, _b) -> (a, []))

setData :: BSL.ByteString -> MonadStore ()
setData x = modify (\(_, b) -> (Just x, b))

clearData :: MonadStore ()
clearData = modify (\(_, b) -> (Nothing, b))

getStoreDir :: MonadStore FilePath
getStoreDir = storeDir <$> ask
