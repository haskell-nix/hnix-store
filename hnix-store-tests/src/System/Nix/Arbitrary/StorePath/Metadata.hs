-- due to recent generic-arbitrary
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.StorePath.Metadata where

import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.Signature ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.StorePath (StorePath)
import System.Nix.StorePath.Metadata (Metadata, StorePathTrust)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck.Instances ()

deriving via GenericArbitrary StorePathTrust
  instance Arbitrary StorePathTrust

deriving via GenericArbitrary (Metadata StorePath)
  instance Arbitrary (Metadata StorePath)

