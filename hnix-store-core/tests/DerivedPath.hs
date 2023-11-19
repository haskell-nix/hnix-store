module DerivedPath where

import Data.Default.Class (Default(def))
import Test.Tasty.QuickCheck
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..))

import qualified Data.Set
import qualified System.Nix.DerivedPath

prop_derivedPathRoundTrip :: Property
prop_derivedPathRoundTrip = forAll (arbitrary `suchThat` nonEmptyOutputsSpec_Names) $ \p ->
  System.Nix.DerivedPath.parseDerivedPath def
    (System.Nix.DerivedPath.derivedPathToText def p)
  === pure p
  where
    nonEmptyOutputsSpec_Names :: DerivedPath -> Bool
    nonEmptyOutputsSpec_Names (DerivedPath_Built _ (OutputsSpec_Names set)) =
      not $ Data.Set.null set
    nonEmptyOutputsSpec_Names _ = True
