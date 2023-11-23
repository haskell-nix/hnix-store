module System.Nix.Store.Remote.Types.SubstituteMode
  ( SubstituteMode(..)
  ) where

import GHC.Generics

-- | Path substitution mode, used by @queryValidPaths@
data SubstituteMode
  = SubstituteMode_DoSubstitute
  | SubstituteMode_DontSubstitute
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
