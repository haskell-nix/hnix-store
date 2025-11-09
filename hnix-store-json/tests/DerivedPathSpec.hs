{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DerivedPathSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..), SingleDerivedPath(..))
import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.StorePath (parseBasePathFromText)

-- upstream-nix/src/libstore-tests/data/derived-path/multi_opaque.json
upstreamMultiOpaque :: DerivedPath
upstreamMultiOpaque = DerivedPath_Opaque $
  forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"

-- upstream-nix/src/libstore-tests/data/derived-path/mutli_built.json
upstreamMultiBuilt :: DerivedPath
upstreamMultiBuilt = DerivedPath_Built
  (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
  (OutputsSpec_Names $ Data.Set.fromList
    [ forceRight $ System.Nix.OutputName.mkOutputName "bar"
    , forceRight $ System.Nix.OutputName.mkOutputName "baz"
    ])

-- upstream-nix/src/libstore-tests/data/derived-path/multi_built_built.json
upstreamMultiBuiltBuilt :: DerivedPath
upstreamMultiBuiltBuilt = DerivedPath_Built
  (SingleDerivedPath_Built
    (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
    (forceRight $ System.Nix.OutputName.mkOutputName "bar"))
  (OutputsSpec_Names $ Data.Set.fromList
    [ forceRight $ System.Nix.OutputName.mkOutputName "baz"
    , forceRight $ System.Nix.OutputName.mkOutputName "quux"
    ])

-- upstream-nix/src/libstore-tests/data/derived-path/multi_built_built_wildcard.json
upstreamMultiBuiltBuiltWildcard :: DerivedPath
upstreamMultiBuiltBuiltWildcard = DerivedPath_Built
  (SingleDerivedPath_Built
    (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
    (forceRight $ System.Nix.OutputName.mkOutputName "bar"))
  OutputsSpec_All

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    it "parses multi_opaque.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/multi_opaque.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamMultiOpaque

    it "parses mutli_built.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/mutli_built.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamMultiBuilt

    it "parses multi_built_built.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/multi_built_built.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamMultiBuiltBuilt

    it "parses multi_built_built_wildcard.json" $ do
      path <- getDataFileName "upstream-libstore-data/derived-path/multi_built_built_wildcard.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamMultiBuiltBuiltWildcard
