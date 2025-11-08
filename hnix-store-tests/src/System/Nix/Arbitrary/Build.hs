-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Build where

import Data.Time (UTCTime)
import Data.Text.Arbitrary ()
import Test.QuickCheck (Arbitrary(..), suchThat)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.OutputName ()
import System.Nix.Arbitrary.Realisation ()
import System.Nix.Arbitrary.UTCTime ()

import System.Nix.Build

import Data.Time.Clock.POSIX qualified

deriving via GenericArbitrary BuildMode
  instance Arbitrary BuildMode

deriving via GenericArbitrary BuildSuccessStatus
  instance Arbitrary BuildSuccessStatus

deriving via GenericArbitrary BuildFailureStatus
  instance Arbitrary BuildFailureStatus

deriving via GenericArbitrary BuildSuccess
  instance Arbitrary BuildSuccess

deriving via GenericArbitrary BuildFailure
  instance Arbitrary BuildFailure

instance Arbitrary BuildResult where
  arbitrary = do
    buildResultStatus <- arbitrary
    buildResultTimesBuilt <- arbitrary `suchThat` (/= 0)
    buildResultStartTime <- arbitrary `suchThat` (/= t0)
    buildResultStopTime <- arbitrary `suchThat` (/= t0)
    buildResultCpuUser <- arbitrary
    buildResultCpuSystem <- arbitrary

    pure BuildResult{..}
    where
      t0 :: UTCTime
      t0 = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
