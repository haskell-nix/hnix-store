{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module System.Nix.Store.Remote.Types (
    MonadStore
  , Logger(..)
  , Field(..)
  , getLog
  , flushLog
  , gotError
  , getError) where


import           System.Nix.Internal.StorePath
import qualified Data.ByteString.Lazy      as LBS
import           Network.Socket            (Socket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

type MonadStore (storeDir :: StoreDir) a = ExceptT String (StateT [Logger] (ReaderT Socket IO)) a
-- type AMonadStore s a = forall storeDir. MonadStore storeDir a

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

gotError :: MonadStore s Bool
gotError = any isError <$> get

getError :: MonadStore s [Logger]
getError = filter isError <$> get

getLog :: MonadStore s [Logger]
getLog = get

flushLog :: MonadStore s ()
flushLog = put []
