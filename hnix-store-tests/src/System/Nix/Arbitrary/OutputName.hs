{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.OutputName where

import System.Nix.OutputName (OutputName)
import System.Nix.OutputName qualified
import System.Nix.StorePath qualified

import Test.QuickCheck (Arbitrary(arbitrary))
import System.Nix.Arbitrary.StorePath ()

instance Arbitrary OutputName where
  arbitrary =
    either (error . show) id
    . System.Nix.OutputName.mkOutputName
    . System.Nix.StorePath.unStorePathName
    <$> arbitrary
