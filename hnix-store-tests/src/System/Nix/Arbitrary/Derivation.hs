{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Derivation where

import Data.Map.Monoidal
import Data.These
import Data.Text (Text)
import Data.Text.Arbitrary ()
import Data.Vector.Arbitrary ()
import System.Nix.Derivation
import System.Nix.StorePath (StorePath)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (Arg, GenericArbitrary(..))
import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.Arbitrary.OutputName ()

deriving via GenericArbitrary
    (Derivation' inputs output)
  instance
    ( Arbitrary inputs
    , Arbitrary output
    , Arg (Derivation' inputs output) inputs
    , Arg (Derivation' inputs output) output
    ) => Arbitrary (Derivation' inputs output)

deriving via GenericArbitrary DerivationOutput
  instance Arbitrary DerivationOutput

deriving via GenericArbitrary DerivationInputs
  instance Arbitrary DerivationInputs

deriving via GenericArbitrary DerivedPathMap
  instance Arbitrary DerivedPathMap

deriving via GenericArbitrary ChildNode
  instance Arbitrary ChildNode

-- TODO these belong elsewhere

deriving newtype instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (MonoidalMap k v)

deriving via GenericArbitrary (These a b)
  instance ( Arg (These a b) a
           , Arg (These a b) b
           , Arbitrary a
           , Arbitrary b
           ) => Arbitrary (These a b)
