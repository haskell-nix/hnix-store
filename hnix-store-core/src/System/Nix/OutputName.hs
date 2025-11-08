{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-|
Description : Derived path output names
-}

module System.Nix.OutputName
  ( OutputName(..)
  , mkOutputName
  , outputStoreObjectName
  -- * Re-exports
  , System.Nix.StorePath.InvalidNameError(..)
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.StorePath
  ( StorePathName
  , InvalidNameError(..)
  , mkStorePathName
  , unStorePathName
  )

-- | Name of the derived path output
-- Typically used for "dev", "doc" sub-outputs
newtype OutputName = OutputName
  { -- | Extract the contents of the name.
    unOutputName :: StorePathName
  } deriving (Eq, Generic, Hashable, Ord, Show)

mkOutputName :: Text -> Either InvalidNameError OutputName
mkOutputName = fmap OutputName . System.Nix.StorePath.mkStorePathName

-- | Compute the name of an output (store object) from the output name
-- and the derivation name
--
-- Invalid character errors are not possible, but total length errors are
outputStoreObjectName :: StorePathName -> OutputName -> Either InvalidNameError StorePathName
outputStoreObjectName drvName outputName = case outputNameS of
  "out" -> Right drvName
  _  -> System.Nix.StorePath.mkStorePathName $
      unStorePathName drvName
      <> "-"
      <> outputNameS
  where
    outputNameS = unStorePathName $ unOutputName outputName
