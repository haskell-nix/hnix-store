-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Build where

import Data.Text.Arbitrary ()
import Test.QuickCheck (Arbitrary(..), suchThat)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.UTCTime ()

import System.Nix.Build

deriving via GenericArbitrary BuildMode
  instance Arbitrary BuildMode

deriving via GenericArbitrary BuildStatus
  instance Arbitrary BuildStatus

instance Arbitrary BuildResult where
  arbitrary = do
    status <- arbitrary
    -- we encode empty errorMessage as Nothing
    errorMessage <- arbitrary `suchThat` (/= Just mempty)
    timesBuilt <- arbitrary
    isNonDeterministic <- arbitrary
    startTime <- arbitrary
    stopTime <- arbitrary

    pure $ BuildResult{..}
