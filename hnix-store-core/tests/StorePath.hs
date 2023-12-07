{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module StorePath where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Text

import System.Nix.StorePath (parseNameText, InvalidNameError(..))

spec_storePath :: Spec
spec_storePath = do
  describe "parseNameText" $ do
    it "parses valid name" $
      parseNameText "name-dev.dotok"
      `shouldBe`
      pure "name-dev.dotok"

    it "fails on empty" $
      parseNameText mempty
      `shouldBe`
      Left EmptyName

    it "fails on too long" $
      parseNameText (Data.Text.replicate 256 "n")
      `shouldBe`
      Left (NameTooLong 256)

    it "fails on leading dot" $
      parseNameText ".ab"
      `shouldBe`
      Left LeadingDot

    it "fails on invalid characters" $
      parseNameText "ab!cd#@"
      `shouldBe`
      Left (InvalidCharacters "!#@")
