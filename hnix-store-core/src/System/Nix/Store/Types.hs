module System.Nix.Store.Types
  ( FileIngestionMethod(..)
  , RepairMode(..)
  ) where

import GHC.Generics (Generic)

-- | Add path recursively or not
data FileIngestionMethod
  = FileIngestionMethod_Flat
  | FileIngestionMethod_FileRecursive
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)

-- | Repair mode
data RepairMode
  = RepairMode_DoRepair
  | RepairMode_DontRepair
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
