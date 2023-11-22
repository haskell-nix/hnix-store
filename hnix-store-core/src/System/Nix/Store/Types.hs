module System.Nix.Store.Types
  ( FileIngestionMethod(..)
  ) where

import GHC.Generics (Generic)

-- | Add path recursively or not
data FileIngestionMethod
  = FileIngestionMethod_Flat
  | FileIngestionMethod_FileRecursive
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
