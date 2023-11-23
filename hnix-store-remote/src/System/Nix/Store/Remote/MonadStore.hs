module System.Nix.Store.Remote.MonadStore
  ( MonadStore
  , mapStoreDir
  , getStoreDir
  , getStoreDir'
  , getLog
  , flushLog
  , gotError
  , getError
  , setData
  , clearData
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (StateT, gets, modify)
import Data.ByteString (ByteString)

import Control.Monad.Trans.State.Strict (mapStateT)
import Control.Monad.Trans.Except (mapExceptT)
import Control.Monad.Trans.Reader (withReaderT)

import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Types.Logger (Logger, isError)
import System.Nix.Store.Remote.Types.StoreConfig (StoreConfig(..))

-- | Ask for a @StoreDir@
getStoreDir' :: (HasStoreDir r, MonadReader r m) => m StoreDir
getStoreDir' = asks hasStoreDir

type MonadStore a
  = ExceptT
      String
      (StateT (Maybe ByteString, [Logger]) (ReaderT StoreConfig IO))
      a

-- | For lying about the store dir in tests
mapStoreDir :: (StoreDir -> StoreDir) -> (MonadStore a -> MonadStore a)
mapStoreDir f = mapExceptT . mapStateT . withReaderT
  $ \c@StoreConfig { storeConfig_dir = sd } -> c { storeConfig_dir = f sd }

gotError :: MonadStore Bool
gotError = gets (any isError . snd)

getError :: MonadStore [Logger]
getError = gets (filter isError . snd)

getLog :: MonadStore [Logger]
getLog = gets snd

flushLog :: MonadStore ()
flushLog = modify (\(a, _b) -> (a, []))

setData :: ByteString -> MonadStore ()
setData x = modify (\(_, b) -> (Just x, b))

clearData :: MonadStore ()
clearData = modify (\(_, b) -> (Nothing, b))

getStoreDir :: MonadStore StoreDir
getStoreDir = asks storeConfig_dir
