module ContentAddressSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)
import System.Nix.Arbitrary ()

import qualified System.Nix.ContentAddress

spec :: Spec
spec = do
  describe "ContentAddress" $ do
    prop "roundtrips" $
      roundtrips
        System.Nix.ContentAddress.buildContentAddress
        System.Nix.ContentAddress.parseContentAddress
