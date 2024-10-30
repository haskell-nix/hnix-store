module System.Nix.Store.Types
  ( FileIngestionMethod(..)
  , PathFilter(..)
  , RepairMode(..)
  ) where

import GHC.Generics (Generic)

-- | Add path recursively or not
data FileIngestionMethod
  = FileIngestionMethod_Flat
  | FileIngestionMethod_NixArchive
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)

-- | Path filtering function
newtype PathFilter = PathFilter
  { pathFilterFunction :: FilePath -> Bool
  }

-- | Repair mode
data RepairMode
  = RepairMode_DoRepair
  | RepairMode_DontRepair
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
