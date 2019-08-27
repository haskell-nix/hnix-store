module System.Nix.Store.Remote.Types (
    MonadStore
  , Error(..)
  , Logger(..)
  , Field(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy      as LBS
import           Network.Socket            (Socket)
import           Pipes

data Error =
    LogError Int LBS.ByteString
  | ParseError String
  | ConnError String
  deriving (Eq, Show)

type MonadStore a = ExceptT Error (Producer Logger (ReaderT Socket IO)) a

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
