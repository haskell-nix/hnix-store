module DerivedPathSpec where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary), forAll, suchThat)

import System.Nix.Arbitrary ()
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..))

import qualified Data.Set
import qualified System.Nix.DerivedPath

spec :: Spec
spec = do
  describe "DerivedPath" $ do
    prop "roundtrips" $ \sd ->
      forAll (arbitrary `suchThat` nonEmptyOutputsSpec_Names) $ \p ->
        System.Nix.DerivedPath.parseDerivedPath sd
          (System.Nix.DerivedPath.derivedPathToText sd p)
        `shouldBe` pure p
  where
    nonEmptyOutputsSpec_Names :: DerivedPath -> Bool
    nonEmptyOutputsSpec_Names (DerivedPath_Built _ (OutputsSpec_Names set)) =
      not $ Data.Set.null set
    nonEmptyOutputsSpec_Names _ = True
