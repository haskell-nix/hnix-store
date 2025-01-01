{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.These
import Data.Text (Text)
import Data.Vector (Vector)
import System.FilePath
import Nix.Derivation
    ( Derivation(..)
    , DerivationOutput(..)
    , DerivationInputs(..)
    , DerivedPathMap(..)
    )
import Prelude hiding (FilePath, either)
import Test.QuickCheck (Arbitrary(..), Gen, oneof)

import Data.Attoparsec.Text.Lazy qualified
import Data.Text qualified
import Data.Text.Lazy.Builder qualified
import Data.Vector qualified
import Nix.Derivation qualified
import Test.QuickCheck qualified

instance Arbitrary Text where
    arbitrary = fmap Data.Text.pack arbitrary

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = fmap Data.Vector.fromList arbitrary

instance Arbitrary (DerivationOutput FilePath Text) where
    arbitrary = oneof
      [ inputAddressedDerivationOutput
      , fixedDerivationOutput
      , contentAddressedDerivationOutput
      ]

inputAddressedDerivationOutput :: Gen (DerivationOutput FilePath Text)
inputAddressedDerivationOutput = do
  path     <- arbitrary
  return (InputAddressedDerivationOutput {..})

fixedDerivationOutput :: Gen (DerivationOutput FilePath Text)
fixedDerivationOutput = do
  path     <- arbitrary
  hashAlgo <- arbitrary
  hash     <- arbitrary
  return (FixedDerivationOutput {..})

contentAddressedDerivationOutput :: Gen (DerivationOutput FilePath Text)
contentAddressedDerivationOutput = do
  hashAlgo <- arbitrary
  return (ContentAddressedDerivationOutput {..})

instance Arbitrary (DerivationInputs FilePath Text) where
    arbitrary = do
        drvs <- arbitrary
        srcs <- arbitrary
        pure DerivationInputs {..}

instance Arbitrary (DerivedPathMap FilePath Text) where
    arbitrary = DerivedPathMap . fmap This <$> arbitrary

instance Arbitrary
  (Derivation
    FilePath
    Text
    Text
    (DerivationOutput FilePath Text)
    (DerivationInputs FilePath Text)
  ) where
    arbitrary = do
        outputs   <- arbitrary
        inputs    <- arbitrary
        platform  <- arbitrary
        builder   <- arbitrary
        args      <- arbitrary
        env       <- arbitrary
        pure Derivation {..}

property
  :: Derivation
     FilePath
     Text
     Text
     (DerivationOutput FilePath Text)
     (DerivationInputs FilePath Text)
  -> Bool
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
