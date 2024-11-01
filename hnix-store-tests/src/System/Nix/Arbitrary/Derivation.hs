-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Derivation where

import Data.Text (Text)
import Data.Text.Arbitrary ()
import Data.Vector.Arbitrary ()
import System.Nix.Derivation
import System.Nix.OutputName (OutputName)
import System.Nix.StorePath (StorePath)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.DerivedPath ()
import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.OutputName ()
import System.Nix.Arbitrary.StorePath ()

deriving via GenericArbitrary (Derivation StorePath Text OutputName DerivationOutput DerivationInputs)
  instance Arbitrary (Derivation StorePath Text OutputName DerivationOutput DerivationInputs)

deriving via GenericArbitrary (DerivationInputs StorePath OutputName)
  instance Arbitrary (DerivationInputs StorePath OutputName)

deriving via GenericArbitrary (DerivationOutput StorePath)
  instance Arbitrary (DerivationOutput StorePath)
