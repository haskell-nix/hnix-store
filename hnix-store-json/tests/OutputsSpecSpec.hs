{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OutputsSpecSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.DerivedPath (OutputsSpec(..))
import System.Nix.JSON ()
import System.Nix.OutputName qualified

-- upstream-nix/src/libstore-tests/data/outputs-spec/all.json
upstreamAll :: OutputsSpec
upstreamAll = OutputsSpec_All

-- upstream-nix/src/libstore-tests/data/outputs-spec/name.json
upstreamName :: OutputsSpec
upstreamName = OutputsSpec_Names $
  Data.Set.singleton $ forceRight $ System.Nix.OutputName.mkOutputName "a"

-- upstream-nix/src/libstore-tests/data/outputs-spec/names.json
upstreamNames :: OutputsSpec
upstreamNames = OutputsSpec_Names $ Data.Set.fromList
  [ forceRight $ System.Nix.OutputName.mkOutputName "a"
  , forceRight $ System.Nix.OutputName.mkOutputName "b"
  ]

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    it "parses all.json" $ do
      path <- getDataFileName "upstream-libstore-data/outputs-spec/all.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamAll

    it "parses name.json" $ do
      path <- getDataFileName "upstream-libstore-data/outputs-spec/name.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamName

    it "parses names.json" $ do
      path <- getDataFileName "upstream-libstore-data/outputs-spec/names.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamNames
