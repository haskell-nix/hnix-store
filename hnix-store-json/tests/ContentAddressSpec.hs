{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ContentAddressSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.ContentAddress (ContentAddress(..), ContentAddressMethod(..))
import System.Nix.Hash (mkNamedDigest)
import System.Nix.JSON ()

-- upstream-nix/src/libstore-tests/data/content-address/nar.json
upstreamNar :: ContentAddress
upstreamNar = ContentAddress
  ContentAddressMethod_NixArchive
  (forceRight $ mkNamedDigest "sha256" "9vLqj0XYoFfJVmoz+ZR02i5camYE1zYSFlDicwxvsKM=")

-- upstream-nix/src/libstore-tests/data/content-address/text.json
upstreamText :: ContentAddress
upstreamText = ContentAddress
  ContentAddressMethod_Text
  (forceRight $ mkNamedDigest "sha256" "8OTC92xYkW7CWPJGhRvqCR0U1CR6L8PhhpRGGxgW4Ts=")

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    it "parses nar.json" $ do
      path <- getDataFileName "upstream-libstore-data/content-address/nar.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamNar

    it "parses text.json" $ do
      path <- getDataFileName "upstream-libstore-data/content-address/text.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamText
