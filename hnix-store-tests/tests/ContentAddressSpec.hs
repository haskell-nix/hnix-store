module ContentAddressSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)
import System.Nix.Arbitrary ()

import System.Nix.ContentAddress qualified

spec :: Spec
spec = do
  describe "ContentAddress" $ do
    prop "roundtrips" $
      roundtrips
        System.Nix.ContentAddress.buildContentAddress
        System.Nix.ContentAddress.parseContentAddress
