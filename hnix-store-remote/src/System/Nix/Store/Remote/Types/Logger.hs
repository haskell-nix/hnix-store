module System.Nix.Store.Remote.Types.Logger
  ( Field(..)
  , Logger(..)
  , isError
  , ActivityID(..)
  -- to be nuked/newtyped
  , ActivityType
  , ResultType
  ) where

import Data.ByteString (ByteString)
import GHC.Generics
import System.Nix.Store.Remote.Types.Verbosity (Verbosity)

newtype ActivityID = ActivityID { unActivityID :: Int }
  deriving (Eq, Generic, Ord, Show)

type ActivityType = Int
type ResultType = Int

data Field
  = Field_LogStr ByteString
  | Field_LogInt Int
  deriving (Eq, Generic, Ord, Show)

data Logger =
    Next ByteString
  | Read Int         -- data needed from source
  | Write ByteString -- data for sink
  | Last
  | Error
      { errorExitStatus :: Int
      , errorMessage :: ByteString
      }
  | StartActivity
      { startActivityID :: ActivityID
      , startActivityVerbosity :: Verbosity
      , startActivityType :: ActivityType
      , startActivityString :: ByteString
      , startActivityFields :: [Field]
      , startActivityParentID :: ActivityID
      }
  | StopActivity
      { stopActivityID :: ActivityID
      }
  | Result
      { resultActivityID :: ActivityID
      , resultType :: ResultType
      , resultFields :: [Field]
      }
  deriving (Eq, Generic, Ord, Show)

isError :: Logger -> Bool
isError (Error _ _) = True
isError _           = False
