module System.Nix.Store.Remote.Types (
    MonadStore
  , NixRemoteError(..)
  , ActivityID
  , ActivityParentID
  , ActivityType
  , Verbosity
  , ResultType
  , Logger(..)
  , Field(..)
  , flushLog
  ) where

import Control.Exception

import qualified Data.ByteString.Lazy      as LBS
import           Data.DList
import           Network.Socket            (Socket)
import           Control.Monad.Reader
import           Control.Monad.State

type MonadStore a = StateT (DList Logger) (ReaderT Socket IO) a

-- | Exceptions that may be thrown while interacting with the nix-daemon.
data NixRemoteError = ParseError String
                    | ProtocolError Int String
                    deriving (Show)

instance Exception NixRemoteError

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

flushLog :: MonadStore ()
flushLog = put mempty
