module System.Nix.Store.Remote.Types
  ( MonadStore
  , StoreConfig(..)
  , CheckFlag
  , doCheck
  , dontCheck
  , unCheckFlag
  , SubstituteFlag
  , doSubstitute
  , dontSubstitute
  , unSubstituteFlag
  , Logger(..)
  , Field(..)
  , mapStoreDir
  , getStoreDir
  , getStoreDir'
  , getLog
  , flushLog
  , gotError
  , getError
  , setData
  , clearData
  , module System.Nix.Store.Remote.Types.ProtoVersion
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (StateT, gets, modify)
import Data.ByteString (ByteString)
import Network.Socket (Socket)

import qualified Data.ByteString.Lazy          as BSL

import Control.Monad.Trans.State.Strict (mapStateT)
import Control.Monad.Trans.Except (mapExceptT)
import Control.Monad.Trans.Reader (withReaderT)

import System.Nix.Store.Remote.Types.ProtoVersion
import System.Nix.StorePath (HasStoreDir(..), StoreDir)


data StoreConfig = StoreConfig
  { storeDir    :: StoreDir
  , storeSocket :: Socket
  }

-- | Check flag, used by @verifyStore@
newtype CheckFlag = CheckFlag { unCheckFlag :: Bool }
  deriving (Eq, Ord, Show)

doCheck, dontCheck :: CheckFlag
doCheck = CheckFlag True
dontCheck = CheckFlag False

-- | Substitute flag, used by @queryValidPaths@
newtype SubstituteFlag = SubstituteFlag { unSubstituteFlag :: Bool }
  deriving (Eq, Ord, Show)

doSubstitute, dontSubstitute :: SubstituteFlag
doSubstitute = SubstituteFlag True
dontSubstitute = SubstituteFlag False

-- | Ask for a @StoreDir@
getStoreDir' :: (HasStoreDir r, MonadReader r m) => m StoreDir
getStoreDir' = asks hasStoreDir

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
