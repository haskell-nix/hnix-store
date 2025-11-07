-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Build where

import Data.Time (UTCTime)
import Data.Text.Arbitrary ()
import Test.QuickCheck (Arbitrary(..), scale, suchThat)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.OutputName ()
import System.Nix.Arbitrary.Realisation ()
import System.Nix.Arbitrary.UTCTime ()

import System.Nix.Build

import Data.Time.Clock.POSIX qualified

deriving via GenericArbitrary BuildMode
  instance Arbitrary BuildMode

deriving via GenericArbitrary BuildStatus
  instance Arbitrary BuildStatus

instance Arbitrary BuildResult where
  arbitrary = do
    buildResultStatus <- arbitrary
    buildResultErrorMessage <- arbitrary
    buildResultTimesBuilt <- arbitrary `suchThat` (/= Just 0)
    buildResultIsNonDeterministic <- arbitrary  `suchThat` (/= Nothing)
    buildResultStartTime <- arbitrary `suchThat` (/= Just t0)
    buildResultStopTime <- arbitrary `suchThat` (/= Just t0)
    buildResultBuiltOutputs <- scale (`div` 10) (arbitrary `suchThat` (/= Nothing))

    pure BuildResult{..}
    where
      t0 :: UTCTime
      t0 = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
