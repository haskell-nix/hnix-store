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
import Test.QuickCheck.Arbitrary.Generic (Arg, GenericArbitrary(..))
import System.Nix.Arbitrary.StorePath ()

deriving via GenericArbitrary
    (Derivation 
      StorePath
      Text
      Text
      (DerivationOutput StorePath Text)
      inputs)
  instance 
    ( Arbitrary inputs
    , Arg
       (Derivation
         StorePath
         Text
         Text
         (DerivationOutput StorePath Text)
         inputs)
       inputs
    ) => Arbitrary
    (Derivation 
      StorePath
      Text
      Text
      (DerivationOutput StorePath Text)
      inputs)

deriving via GenericArbitrary (DerivationOutput StorePath Text)
  instance Arbitrary (DerivationOutput StorePath Text)

deriving via GenericArbitrary (DerivationInputs StorePath Text)
  instance Arbitrary (DerivationInputs StorePath Text)
