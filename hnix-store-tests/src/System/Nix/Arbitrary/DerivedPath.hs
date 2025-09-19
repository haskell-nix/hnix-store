-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.DerivedPath where

import Data.Set qualified
import Test.QuickCheck
import System.Nix.Arbitrary.OutputName ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.DerivedPath

instance Arbitrary OutputsSpec where
  arbitrary = oneof
    [ pure OutputsSpec_All
    ,   OutputsSpec_Names
      . Data.Set.fromList
      <$> ((:) <$> arbitrary <*> arbitrary)
    ]

arbitrarySingleDerivedPath :: Word -> Gen SingleDerivedPath
arbitrarySingleDerivedPath = \case
  0 -> SingleDerivedPath_Opaque <$> arbitrary
  depth -> SingleDerivedPath_Built
    <$> (arbitrarySingleDerivedPath $ depth - 1)
    <*> arbitrary

arbitraryDerivedPath :: Word -> Gen DerivedPath
arbitraryDerivedPath = \case
  0 -> DerivedPath_Opaque <$> arbitrary
  depth -> DerivedPath_Built
    <$> (arbitrarySingleDerivedPath $ depth - 1)
    <*> arbitrary

instance Arbitrary SingleDerivedPath where
  arbitrary = do
    n <- getSize
    k <- choose (0, n)
    arbitrarySingleDerivedPath $ fromIntegral k

instance Arbitrary DerivedPath where
  arbitrary = do
    n <- getSize
    k <- choose (0, n)
    arbitraryDerivedPath $ fromIntegral k
