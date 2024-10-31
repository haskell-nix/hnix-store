-- | TODO rename module
module System.Nix.Store.Types
  ( PathFilter(..)
  , RepairMode(..)
  ) where

import GHC.Generics (Generic)

-- | Path filtering function
newtype PathFilter = PathFilter
  { pathFilterFunction :: FilePath -> Bool
  }

-- | Repair mode
data RepairMode
  = RepairMode_DoRepair
  | RepairMode_DontRepair
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
