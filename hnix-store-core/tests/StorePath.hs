{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module StorePath where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Data.Either
import qualified Data.Text

import System.Nix.StorePath (mkStorePathName)

spec_storePath :: Spec
spec_storePath = do
  describe "StorePathName" $ do
    it "parses valid name" $
      mkStorePathName "name-dev.dotok"
      `shouldSatisfy`
      Data.Either.isRight

    it "fails on empty" $
      mkStorePathName mempty
      `shouldBe`
      Left EmptyName

    it "fails on too long" $
      mkStorePathName (Data.Text.replicate 256 "n")
      `shouldBe`
      Left (NameTooLong 256)

    it "fails on leading dot" $
      mkStorePathName ".ab"
      `shouldBe`
      Left LeadingDot

    it "fails on invalid characters" $
      mkStorePathName "ab!cd#@"
      `shouldBe`
      Left (InvalidCharacters "!#@")
