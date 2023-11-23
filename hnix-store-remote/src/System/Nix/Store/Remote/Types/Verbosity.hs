module System.Nix.Store.Remote.Types.Verbosity
  ( Verbosity(..)
  ) where

import GHC.Generics

-- | Logging verbosity
data Verbosity
  = Verbosity_Error
  | Verbosity_Warn
  | Verbosity_Notice
  | Verbosity_Info
  | Verbosity_Talkative
  | Verbosity_Chatty
  | Verbosity_Debug
  | Verbosity_Vomit
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)
