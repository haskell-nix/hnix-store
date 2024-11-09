module DerivationSpec where

import Data.Functor.Identity (Identity(..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()
import System.Nix.Derivation
  ( derivationInputsFromSingleDerivedPath
  , derivationInputsToDerivedPaths
  )

spec :: Spec
spec = do
  describe "DerivationInput" $ do
    prop "roundtrips to (Set SingleDerivedPath)" $
      -- Order is important, 'Set SingleDerivedPath' is the normal from,
      -- since the arbitrary instance for 'DerivationInput' doesn't
      -- properly avoid empty child maps.
      roundtrips
        (foldMap derivationInputsFromSingleDerivedPath)
        (Identity . derivationInputsToDerivedPaths)
