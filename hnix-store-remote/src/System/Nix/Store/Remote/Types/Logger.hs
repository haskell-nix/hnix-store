module System.Nix.Store.Remote.Types.Logger
  ( Field(..)
  , Logger(..)
  , isError
  -- to be nuked/newtyped
  , ActivityID
  , ActivityParentID
  , ActivityType
  , Verbosity
  , ResultType
  ) where

import Data.ByteString (ByteString)

type ActivityID = Int
type ActivityParentID = Int
type ActivityType = Int
type Verbosity = Int
type ResultType = Int

data Field = LogStr ByteString | LogInt Int
  deriving (Eq, Ord, Show)

data Logger =
    Next          ByteString
  | Read          Int        -- data needed from source
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


