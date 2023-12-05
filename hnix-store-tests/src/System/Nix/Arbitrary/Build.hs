-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Build where

import Data.Text.Arbitrary ()
import Test.QuickCheck (Arbitrary(..), suchThat)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.OutputName ()
import System.Nix.Arbitrary.Realisation ()
import System.Nix.Arbitrary.UTCTime ()

import System.Nix.Build

deriving via GenericArbitrary BuildMode
  instance Arbitrary BuildMode

deriving via GenericArbitrary BuildStatus
  instance Arbitrary BuildStatus

instance Arbitrary BuildResult where
  arbitrary = do
    buildResultStatus <- arbitrary
    buildResultErrorMessage <- arbitrary
    buildResultTimesBuilt <- arbitrary
    buildResultIsNonDeterministic <- arbitrary
    buildResultStartTime <- arbitrary
    buildResultStopTime <- arbitrary
    buildResultBuiltOutputs <- arbitrary `suchThat` (/= Nothing)

    pure BuildResult{..}

instance Arbitrary OldBuildResult where
  arbitrary = do
    oldBuildResultStatus <- arbitrary
    oldBuildResultErrorMessage <- arbitrary
    oldBuildResultBuiltOutputs <- arbitrary `suchThat` (/= Just mempty)

    pure OldBuildResult{..}


