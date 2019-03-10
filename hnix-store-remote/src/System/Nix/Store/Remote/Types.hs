module System.Nix.Store.Remote.Types (
    MonadStore
  , StoreConfig(..)
  , Logger(..)
  , Field(..)
  , getStoreDir
  , getLog
  , flushLog
  , gotError
  , getError) where


import qualified Data.ByteString.Lazy      as LBS
import           Network.Socket            (Socket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           System.IO                 (Handle)

-- XXX
import           System.Nix.Internal.Path  (StoreDir(..))

data StoreConfig = StoreConfig {
    storeDir        :: StoreDir
  , storeSocket     :: Socket
  }

type MonadStore a = ExceptT String (StateT (Maybe Handle, [Logger]) (ReaderT StoreConfig IO)) a

type ActivityID = Int
type ActivityParentID = Int
type ActivityType = Int
type Verbosity = Int
type ResultType = Int

data Field = LogStr LBS.ByteString | LogInt Int
  deriving (Eq, Ord, Show)

data Logger =
    Next          LBS.ByteString
  | Read          Int            -- data needed from source
  | Write         LBS.ByteString -- data for sink
  | Last
  | Error         Int LBS.ByteString
  | StartActivity ActivityID Verbosity ActivityType LBS.ByteString [Field] ActivityParentID
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

getStoreDir :: MonadStore (StoreDir)
getStoreDir = storeDir <$> ask
