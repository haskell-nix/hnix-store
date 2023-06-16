{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
module System.Nix.Store.Remote.Types
  ( MonadStore
  , StoreConfig(..)
  , Logger(..)
  , Field(..)
  , mapStoreDir
  , getStoreDir
  , getLog
  , flushLog
  , gotError
  , getError
  , setData
  , clearData
  )
where

import           Control.Monad.Trans.State.Strict (mapStateT)
import           Control.Monad.Trans.Except (mapExceptT)
import qualified Data.ByteString.Lazy          as BSL
import           Network.Socket                 ( Socket )

import           System.Nix.StorePath          ( StoreDir )

data StoreConfig = StoreConfig
  { storeDir    :: StoreDir
  , storeSocket :: Socket
  }

type MonadStore a
  = ExceptT
      String
      (StateT (Maybe BSL.ByteString, [Logger]) (ReaderT StoreConfig IO))
      a

-- | For lying about the store dir in tests
mapStoreDir :: (StoreDir -> StoreDir) -> (MonadStore a -> MonadStore a)
mapStoreDir f = mapExceptT . mapStateT . withReaderT $ \c@StoreConfig { storeDir = sd } -> c { storeDir = f sd }

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
gotError = gets (any isError . snd)

getError :: MonadStore [Logger]
getError = gets (filter isError . snd)

getLog :: MonadStore [Logger]
getLog = gets snd

flushLog :: MonadStore ()
flushLog = modify (\(a, _b) -> (a, []))

setData :: BSL.ByteString -> MonadStore ()
setData x = modify (\(_, b) -> (Just x, b))

clearData :: MonadStore ()
clearData = modify (\(_, b) -> (Nothing, b))

getStoreDir :: MonadStore StoreDir
getStoreDir = asks storeDir
