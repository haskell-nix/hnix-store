{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Text (Text)
import Data.Vector (Vector)
import System.FilePath
import Nix.Derivation (Derivation(..), DerivationOutput(..))
import Prelude hiding (FilePath, either)
import Test.QuickCheck (Arbitrary(..))

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

instance Arbitrary (DerivationOutput FilePath Text) where
    arbitrary = do
        path     <- arbitrary
        hashAlgo <- arbitrary
        hash     <- arbitrary
        return (DerivationOutput {..})

instance Arbitrary (Derivation FilePath Text) where
    arbitrary = do
        outputs   <- arbitrary
        inputDrvs <- arbitrary
        inputSrcs <- arbitrary
        platform  <- arbitrary
        builder   <- arbitrary
        args      <- arbitrary
        env       <- arbitrary
        return (Derivation {..})

property :: Derivation FilePath Text -> Bool
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
