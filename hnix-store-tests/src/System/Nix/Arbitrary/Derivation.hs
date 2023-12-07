-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Derivation where

import Data.Text (Text)
import Data.Text.Arbitrary ()
import Data.Vector.Arbitrary ()
import System.Nix.Derivation
import System.Nix.StorePath (StorePath)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.StorePath ()

deriving via GenericArbitrary (Derivation StorePath Text)
  instance Arbitrary (Derivation StorePath Text)
deriving via GenericArbitrary (DerivationOutput StorePath Text)
  instance Arbitrary (DerivationOutput StorePath Text)
