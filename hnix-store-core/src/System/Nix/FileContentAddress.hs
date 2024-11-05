module System.Nix.FileContentAddress
  ( FileIngestionMethod(..)
  ) where

import GHC.Generics (Generic)

data FileIngestionMethod
  = FileIngestionMethod_Flat
  | FileIngestionMethod_NixArchive
  deriving (Bounded, Eq, Generic, Enum, Ord, Show)
