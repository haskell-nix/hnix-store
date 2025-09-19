-- due to Illegal equational constraint Test.QuickCheck.Arbitrary.Generic.TypesDiffer
{-# LANGUAGE TypeFamilies #-}
-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Realisation where

import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.OutputName ()
import System.Nix.Arbitrary.Signature ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.Realisation (BuildTraceKey, Realisation)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (Arg, GenericArbitrary(..), genericArbitrary, genericShrink)

instance
  ( Arg (BuildTraceKey outputName) outputName
  , Arbitrary outputName
  ) =>
  Arbitrary (BuildTraceKey outputName)
  where
    arbitrary = genericArbitrary
    shrink = genericShrink

deriving via GenericArbitrary Realisation
  instance Arbitrary Realisation
