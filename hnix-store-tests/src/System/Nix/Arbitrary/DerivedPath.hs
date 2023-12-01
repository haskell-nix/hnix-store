-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.DerivedPath where

import qualified Data.Set
import Test.QuickCheck (Arbitrary(..), oneof)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import System.Nix.Arbitrary.StorePath ()
import System.Nix.DerivedPath (DerivedPath, OutputsSpec(..))

instance Arbitrary OutputsSpec where
  arbitrary = oneof
    [ pure OutputsSpec_All
    ,   OutputsSpec_Names
      . Data.Set.fromList
      <$> ((:) <$> arbitrary <*> arbitrary)
    ]

deriving via GenericArbitrary DerivedPath
  instance Arbitrary DerivedPath
