{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StorePathSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.JSON ()
import System.Nix.StorePath (StorePath, parseBasePathFromText)

-- upstream-nix/src/libstore-tests/data/store-path/simple.json
upstreamSimple :: StorePath
upstreamSimple = forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    it "parses simple.json" $ do
      path <- getDataFileName "upstream-libstore-data/store-path/simple.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamSimple
