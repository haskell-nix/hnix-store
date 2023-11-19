module ContentAddressSpec where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import System.Nix.Arbitrary ()

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.Builder

import qualified System.Nix.ContentAddress

spec :: Spec
spec = do
  describe "ContentAddress" $ do
    prop "roundtrips" $ \caAddr ->
      Data.Attoparsec.Text.Lazy.parseOnly
        System.Nix.ContentAddress.contentAddressParser
          (Data.Text.Lazy.Builder.toLazyText
            (System.Nix.ContentAddress.contentAddressBuilder caAddr))
      `shouldBe` pure caAddr

