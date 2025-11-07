{-# LANGUAGE DeriveAnyClass #-}
{-|
Description : Derived path output names
-}

module System.Nix.OutputName
  ( OutputName(..)
  , mkOutputName
  -- * Re-exports
  , System.Nix.StorePath.InvalidNameError(..)
  , System.Nix.StorePath.parseNameText
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.StorePath (InvalidNameError)

import System.Nix.StorePath qualified

-- | Name of the derived path output
-- Typically used for "dev", "doc" sub-outputs
newtype OutputName = OutputName
  { -- | Extract the contents of the name.
    unOutputName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

mkOutputName :: Text -> Either InvalidNameError OutputName
mkOutputName = fmap OutputName . System.Nix.StorePath.parseNameText
