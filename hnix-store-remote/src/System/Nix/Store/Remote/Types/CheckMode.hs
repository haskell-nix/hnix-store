module System.Nix.Store.Remote.Types.CheckMode
  ( CheckMode(..)
  ) where

import GHC.Generics

-- | Check mode, used by @verifyStore@
data CheckMode
  = CheckMode_DoCheck
  | CheckMode_DontCheck
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
