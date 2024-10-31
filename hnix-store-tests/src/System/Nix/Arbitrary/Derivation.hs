-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Derivation where

import Data.These
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

deriving via GenericArbitrary (DerivedPathMap StorePath Text)
  instance Arbitrary (DerivedPathMap StorePath Text)

-- TODO this belongs elsewhere
deriving via GenericArbitrary (These a b)
  instance ( Arg (These a b) a
           , Arg (These a b) b
           , Arbitrary a
           , Arbitrary b
           ) => Arbitrary (These a b)
