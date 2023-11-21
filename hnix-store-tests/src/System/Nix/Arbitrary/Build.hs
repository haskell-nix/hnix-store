-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Build where

import System.Nix.Build

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck.Instances ()

deriving via GenericArbitrary BuildMode
  instance Arbitrary BuildMode

deriving via GenericArbitrary BuildStatus
  instance Arbitrary BuildStatus

deriving via GenericArbitrary BuildResult
  instance Arbitrary BuildResult
