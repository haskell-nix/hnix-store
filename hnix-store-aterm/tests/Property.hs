{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Prelude hiding (FilePath, either)

import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck qualified
import Test.QuickCheck.Property (failed, succeeded, Result(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

import System.Nix.StorePath
import System.Nix.Derivation
    ( Derivation'(..)
    , DerivationOutput(..)
    )
import System.Nix.Arbitrary.Derivation ()
import System.Nix.Derivation.ATerm qualified
import System.Nix.Derivation.Traditional

deriving via GenericArbitrary TraditionalDerivationInputs
  instance Arbitrary TraditionalDerivationInputs

property
  :: StoreDir
  -> Derivation'
     TraditionalDerivationInputs
     DerivationOutput
  -> Result
property storeDir derivation0 =
    if either == expected
    then succeeded
    else failed { reason = unlines ["", show either, show expected] }
  where
    builder = System.Nix.Derivation.ATerm.buildDerivation storeDir derivation0

    text = Data.Text.Lazy.Builder.toLazyText builder

    result =
        Data.Attoparsec.Text.Lazy.parse
          (System.Nix.Derivation.ATerm.parseDerivation storeDir (name derivation0))
          text

    either, expected :: Either String (Derivation' TraditionalDerivationInputs DerivationOutput)

    either =
        Data.Attoparsec.Text.Lazy.eitherResult result

    expected = Right derivation0

main :: IO ()
main = Test.QuickCheck.quickCheck property
