-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.ContentAddress where

import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.Store.Types ()
import System.Nix.ContentAddress (ContentAddress, ContentAddressMethod)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

deriving via GenericArbitrary ContentAddressMethod
  instance Arbitrary ContentAddressMethod

deriving via GenericArbitrary ContentAddress
  instance Arbitrary ContentAddress
