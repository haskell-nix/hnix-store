-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Store.Remote.Arbitrary where

import System.Nix.Store.Remote.Types

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck.Instances ()

-- * Logger

deriving via GenericArbitrary Activity
  instance Arbitrary Activity

deriving via GenericArbitrary ActivityID
  instance Arbitrary ActivityID

deriving via GenericArbitrary ActivityResult
  instance Arbitrary ActivityResult

deriving via GenericArbitrary Field
  instance Arbitrary Field

deriving via GenericArbitrary LoggerOpCode
  instance Arbitrary LoggerOpCode

deriving via GenericArbitrary Verbosity
  instance Arbitrary Verbosity
