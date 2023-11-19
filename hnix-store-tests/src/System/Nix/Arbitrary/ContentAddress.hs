-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.ContentAddress where

import System.Nix.Arbitrary.Hash ()
import System.Nix.ContentAddress (FileIngestionMethod, ContentAddress, ContentAddressMethod)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

deriving via GenericArbitrary FileIngestionMethod
  instance Arbitrary FileIngestionMethod

deriving via GenericArbitrary ContentAddressMethod
  instance Arbitrary ContentAddressMethod

deriving via GenericArbitrary ContentAddress
  instance Arbitrary ContentAddress
