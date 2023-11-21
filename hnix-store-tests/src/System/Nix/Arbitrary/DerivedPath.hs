-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.DerivedPath where

import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.StorePath ()
import System.Nix.DerivedPath (DerivedPath, OutputsSpec)

deriving via GenericArbitrary OutputsSpec
  instance Arbitrary OutputsSpec

deriving via GenericArbitrary DerivedPath
  instance Arbitrary DerivedPath
