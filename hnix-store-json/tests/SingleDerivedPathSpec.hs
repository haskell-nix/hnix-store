{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SingleDerivedPathSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.DerivedPath (SingleDerivedPath(..))
import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.StorePath (parseBasePathFromText)

-- upstream-nix/src/libstore-tests/data/derived-path/single_opaque.json
upstreamSingleOpaque :: SingleDerivedPath
upstreamSingleOpaque = SingleDerivedPath_Opaque $
  forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"

-- upstream-nix/src/libstore-tests/data/derived-path/single_built.json
upstreamSingleBuilt :: SingleDerivedPath
upstreamSingleBuilt = SingleDerivedPath_Built
  (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
  (forceRight $ System.Nix.OutputName.mkOutputName "bar")

-- upstream-nix/src/libstore-tests/data/derived-path/single_built_built.json
upstreamSingleBuiltBuilt :: SingleDerivedPath
upstreamSingleBuiltBuilt = SingleDerivedPath_Built
  (SingleDerivedPath_Built
    (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
    (forceRight $ System.Nix.OutputName.mkOutputName "bar"))
  (forceRight $ System.Nix.OutputName.mkOutputName "baz")

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    it "parses single_opaque.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/single_opaque.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamSingleOpaque

    it "parses single_built.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/single_built.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamSingleBuilt

    it "parses single_built_built.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/single_built_built.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamSingleBuiltBuilt
