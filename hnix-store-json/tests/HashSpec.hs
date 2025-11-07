{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HashSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.Hash (mkNamedDigest)
import System.Nix.JSON (HashJSON(..))

-- upstream-nix/src/libutil-tests/data/hash/simple.json
upstreamSimpleHash :: HashJSON
upstreamSimpleHash = HashJSON $ forceRight $ mkNamedDigest "sha256" "8OTC92xYkW7CWPJGhRvqCR0U1CR6L8PhhpRGGxgW4Ts="

-- upstream-nix/src/libutil-tests/data/hash/sha256-base16.json
upstreamSHA256Base16 :: HashJSON
upstreamSHA256Base16 = HashJSON $ forceRight $ mkNamedDigest "sha256" "f0e4c2f76c58916ec258f246851bea091d14d4247a2fc3e18694461b1816e13b"

-- upstream-nix/src/libutil-tests/data/hash/sha256-base64.json
upstreamSHA256Base64 :: HashJSON
upstreamSHA256Base64 = HashJSON $ forceRight $ mkNamedDigest "sha256" "8OTC92xYkW7CWPJGhRvqCR0U1CR6L8PhhpRGGxgW4Ts="

-- upstream-nix/src/libutil-tests/data/hash/sha256-nix32.json
upstreamSHA256Nix32 :: HashJSON
upstreamSHA256Nix32 = HashJSON $ forceRight $ mkNamedDigest "sha256" "0fz12qc1nillhvhw6bvs4ka18789x8dqaipjb316x4aqdkvw5r7h"

spec :: Spec
spec = do
  describe "Hash" $ do
    describe "upstream Nix test data" $ do
      it "parses simple.json" $ do
        path <- getDataFileName "upstream-libutil-data/hash/simple.json"
        json <- BSL.readFile path
        eitherDecode json `shouldBe` Right upstreamSimpleHash

      it "parses sha256-base16.json" $ do
        path <- getDataFileName "upstream-libutil-data/hash/sha256-base16.json"
        json <- BSL.readFile path
        eitherDecode json `shouldBe` Right upstreamSHA256Base16

      it "parses sha256-base64.json" $ do
        path <- getDataFileName "upstream-libutil-data/hash/sha256-base64.json"
        json <- BSL.readFile path
        eitherDecode json `shouldBe` Right upstreamSHA256Base64

      it "parses sha256-nix32.json" $ do
        path <- getDataFileName "upstream-libutil-data/hash/sha256-nix32.json"
        json <- BSL.readFile path
        eitherDecode json `shouldBe` Right upstreamSHA256Nix32
