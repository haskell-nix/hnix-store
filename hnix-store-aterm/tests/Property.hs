{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Prelude hiding (FilePath, either)

import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck qualified
import Test.QuickCheck.Property (failed, succeeded, Result(..))
import Test.QuickCheck.Arbitrary.Generic (Arg, GenericArbitrary(..))

import System.Nix.StorePath
import System.Nix.Arbitrary.Derivation ()
import System.Nix.Derivation
import System.Nix.Derivation.ATerm qualified
import System.Nix.Derivation.Traditional

deriving via GenericArbitrary TraditionalDerivationInputs
  instance Arbitrary TraditionalDerivationInputs

deriving via GenericArbitrary (TraditionalDerivation' inputs outputs)
  instance
    ( Arbitrary inputs
    , Arbitrary outputs
    , Arg (TraditionalDerivation' inputs outputs) inputs
    , Arg (TraditionalDerivation' inputs outputs) outputs
    ) => Arbitrary (TraditionalDerivation' inputs outputs)

property
  :: StoreDir
  -> TraditionalDerivation'
     TraditionalDerivationInputs
     FreeformDerivationOutputs
  -> Result
property storeDir derivation0 =
    if either == expected
    then succeeded
    else failed { reason = unlines ["", show either, show expected] }
  where
    builder = System.Nix.Derivation.ATerm.buildTraditionalDerivation storeDir derivation0

    text = Data.Text.Lazy.Builder.toLazyText builder

    result =
        Data.Attoparsec.Text.Lazy.parse
          (System.Nix.Derivation.ATerm.parseTraditionalDerivation storeDir)
          text

    either, expected :: Either String (TraditionalDerivation' TraditionalDerivationInputs FreeformDerivationOutputs)

    either =
        Data.Attoparsec.Text.Lazy.eitherResult result

    expected = Right derivation0

main :: IO ()
main = Test.QuickCheck.quickCheck property
