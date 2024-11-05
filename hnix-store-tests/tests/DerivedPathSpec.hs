module DerivedPathSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()

import qualified System.Nix.DerivedPath

spec :: Spec
spec = do
  describe "SingleDerivedPath" $ do
    prop "roundtrips" $ \sd ->
      roundtrips
        (System.Nix.DerivedPath.singleDerivedPathToText sd)
        (System.Nix.DerivedPath.parseSingleDerivedPath sd)

  describe "DerivedPath" $ do
    prop "roundtrips" $ \sd ->
      roundtrips
        (System.Nix.DerivedPath.derivedPathToText sd)
        (System.Nix.DerivedPath.parseDerivedPath sd)
