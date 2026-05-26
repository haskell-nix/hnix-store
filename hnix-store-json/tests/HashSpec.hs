{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HashSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.Hash (mkNamedDigest)
import System.Nix.JSON (HashJSON(..))

-- upstream-nix/src/libutil-tests/data/hash/sha256.json
upstreamSHA256 :: HashJSON
upstreamSHA256 = HashJSON $ forceRight $ mkNamedDigest "sha256" "8OTC92xYkW7CWPJGhRvqCR0U1CR6L8PhhpRGGxgW4Ts="

-- upstream-nix/src/libutil-tests/data/hash/sha512.json
-- Note: sha512.json also contains a sha256 SRI hash (same test value as sha256.json)
upstreamSHA512 :: HashJSON
upstreamSHA512 = HashJSON $ forceRight $ mkNamedDigest "sha256" "8OTC92xYkW7CWPJGhRvqCR0U1CR6L8PhhpRGGxgW4Ts="

spec :: Spec
spec = do
  let dir = "upstream-libutil-data/hash"
  describe "upstream Nix test data" $ do
    parsesUpstream dir "sha256.json" upstreamSHA256
    parsesUpstream dir "sha512.json" upstreamSHA512
