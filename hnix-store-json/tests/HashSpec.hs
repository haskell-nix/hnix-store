{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HashSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

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
  let dir = "upstream-libutil-data/hash"
  describe "upstream Nix test data" $ do
    parsesUpstream dir "simple.json" upstreamSimpleHash
    parsesUpstream dir "sha256-base16.json" upstreamSHA256Base16
    parsesUpstream dir "sha256-base64.json" upstreamSHA256Base64
    parsesUpstream dir "sha256-nix32.json" upstreamSHA256Nix32
