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
    buildResultStatus <- arbitrary
    -- we encode empty errorMessage as Nothing
    buildResultErrorMessage <- arbitrary `suchThat` (/= Just mempty)
    buildResultTimesBuilt <- arbitrary
    buildResultIsNonDeterministic <- arbitrary
    buildResultStartTime <- arbitrary
    buildResultStopTime <- arbitrary

    pure $ BuildResult{..}
