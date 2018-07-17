module System.Nix.Store.Remote.Types (
    MonadStore
  , Logger(..)
  , Field(..)
  , getLog
  , flushLog
  , gotError
  , getError) where


import qualified Data.ByteString.Lazy      as LBS
import           Network.Socket            (Socket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

type MonadStore a = ExceptT String (StateT [Logger] (ReaderT Socket IO)) a

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
gotError = any isError <$> get

getError :: MonadStore [Logger]
getError = filter isError <$> get

getLog :: MonadStore [Logger]
getLog = get

flushLog :: MonadStore ()
flushLog = put []
