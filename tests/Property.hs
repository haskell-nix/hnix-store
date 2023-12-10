{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Text (Text)
import Data.Vector (Vector)
import System.FilePath
import Nix.Derivation
    ( Derivation(..)
    , DerivationInputs(..)
    , DerivationOutput(..)
    )
import Prelude hiding (FilePath, either)
import Test.QuickCheck (Arbitrary(..), Gen, oneof)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Nix.Derivation
import qualified Test.QuickCheck

instance Arbitrary Text where
    arbitrary = fmap Data.Text.pack arbitrary

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = fmap Data.Vector.fromList arbitrary

instance Arbitrary (DerivationInputs FilePath Text) where
    arbitrary = do
        drvs <- arbitrary
        srcs <- arbitrary
        pure DerivationInputs {..}

instance Arbitrary (DerivationOutput FilePath) where
    arbitrary = oneof
      [ derivationOutput
      , fixedDerivationOutput
      , contentAddressedDerivationOutput
      ]

derivationOutput :: Gen (DerivationOutput FilePath)
derivationOutput = do
  path     <- arbitrary
  return (DerivationOutput {..})

fixedDerivationOutput :: Gen (DerivationOutput FilePath)
fixedDerivationOutput = do
  path     <- arbitrary
  hashAlgo <- arbitrary
  hash     <- arbitrary
  return (FixedDerivationOutput {..})

contentAddressedDerivationOutput :: Gen (DerivationOutput FilePath)
contentAddressedDerivationOutput = do
  hashAlgo <- arbitrary
  return (ContentAddressedDerivationOutput {..})

instance Arbitrary (Derivation FilePath Text Text DerivationOutput DerivationInputs) where
    arbitrary = do
        outputs   <- arbitrary
        inputs    <- arbitrary
        platform  <- arbitrary
        builder   <- arbitrary
        args      <- arbitrary
        env       <- arbitrary
        pure Derivation {..}

property :: Derivation FilePath Text Text DerivationOutput DerivationInputs -> Bool
property derivation0 = either == Right derivation0
  where
    builder = Nix.Derivation.buildDerivation derivation0

    text = Data.Text.Lazy.Builder.toLazyText builder

    result =
        Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text

    either =
        Data.Attoparsec.Text.Lazy.eitherResult result

main :: IO ()
main = Test.QuickCheck.quickCheck property
