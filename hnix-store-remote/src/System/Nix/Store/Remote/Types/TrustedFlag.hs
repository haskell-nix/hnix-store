module System.Nix.Store.Remote.Types.TrustedFlag
  ( TrustedFlag(..)
  ) where

import GHC.Generics (Generic)

-- | Whether remote side trust us
data TrustedFlag
  = TrustedFlag_Trusted
  | TrustedFlag_NotTrusted
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
