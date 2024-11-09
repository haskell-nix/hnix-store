{-# LANGUAGE DeriveAnyClass #-}
{-|
Description : Derived path output names
-}

module System.Nix.OutputName
  ( OutputName(..)
  , mkOutputName
  -- * Re-exports
  , System.Nix.StorePath.InvalidNameError(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.StorePath (StorePathName, InvalidNameError(..), mkStorePathName)

-- | Name of the derived path output
-- Typically used for "dev", "doc" sub-outputs
newtype OutputName = OutputName
  { -- | Extract the contents of the name.
    unOutputName :: StorePathName
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance NFData OutputName

mkOutputName :: Text -> Either InvalidNameError OutputName
mkOutputName = fmap OutputName . System.Nix.StorePath.mkStorePathName
