{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Derivation where

import Data.Dependent.Sum
import Data.Either (isRight)
import Data.Map qualified
import Data.Map.Monoidal
import Data.Some
import Data.Text.Arbitrary ()
import Data.These
import Data.Vector.Arbitrary ()
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import System.Nix.Derivation
import System.Nix.ContentAddress
import System.Nix.Hash
import System.Nix.OutputName
import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.Arbitrary.OutputName ()

instance
  ( Arbitrary inputs
  , Arbitrary output
  , Arg (Derivation' inputs output) inputs
  , Arg (Derivation' inputs output) output
  ) => Arbitrary (Derivation' inputs output)
 where
  arbitrary = genericArbitrary `suchThat` \drv ->
    -- ensure output path name is not too long
    all (\on -> isRight $ outputStoreObjectName (name drv) on)
    $ Data.Map.keys (outputs drv)
  shrink = genericShrink

instance Arbitrary DerivationOutput where
  arbitrary = genericArbitrary `suchThat` \case
    InputAddressedDerivationOutput {} -> True
    FixedDerivationOutput {method, hash = hashAlgo :=> _} -> f method hashAlgo
    ContentAddressedDerivationOutput {method, hashAlgo = Some hashAlgo } -> f method hashAlgo
    where
      -- Ensure a valid combination
      f = \case
        ContentAddressMethod_Text -> \case
          HashAlgo_SHA256 -> True
          _ -> False
        _ -> \_ -> True
  shrink = genericShrink

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
