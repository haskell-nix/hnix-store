{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module DerivationSpec where

import Crypto.Hash (SHA256)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Dependent.Sum (DSum(..))
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Some (Some(..))
import Data.These (These(..))
import Data.Vector qualified as Vector
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Nix (forceRight)

import System.Nix.Base (BaseEncoding(..))
import System.Nix.ContentAddress (ContentAddressMethod(..))
import System.Nix.Derivation
  ( ChildNode(..)
  , Derivation
  , Derivation'(..)
  , DerivationInputs(..)
  , DerivedPathMap(..)
  , DerivationType(..)
  , InputAddressedDerivationOutput(..)
  , FixedDerivationOutput(..)
  , ContentAddressedDerivationOutput(..)
  )
import System.Nix.Hash (HashAlgo(..), decodeDigestWith)
import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.StorePath (mkStorePathName, parseBasePathFromText)

simpleDerivation :: Derivation
simpleDerivation = Derivation
  { name = forceRight $ mkStorePathName "simple-derivation"
  , outputs = DerivationType_InputAddressing :=> mempty
  , inputs = DerivationInputs
      { srcs = Set.singleton $ forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-dep1"
      , drvs = DerivedPathMap $ MonoidalMap.singleton
          (forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-dep2.drv")
          (ChildNode $ This $ Set.fromList
            [ forceRight $ System.Nix.OutputName.mkOutputName "cat"
            , forceRight $ System.Nix.OutputName.mkOutputName "dog"
            ])
      }
  , platform = "wasm-sel4"
  , builder = "foo"
  , args = Vector.fromList ["bar", "baz"]
  , env = Map.fromList [("BIG_BAD", "WOLF")]
  }

dynDepDerivation :: Derivation
dynDepDerivation = Derivation
  { name = forceRight $ mkStorePathName "dyn-dep-derivation"
  , outputs = DerivationType_InputAddressing :=> mempty
  , inputs = DerivationInputs
      { srcs = Set.singleton $ forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-dep1"
      , drvs = DerivedPathMap $ MonoidalMap.singleton
          (forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-dep2.drv")
          (ChildNode $ These
            (Set.fromList
              [ forceRight $ System.Nix.OutputName.mkOutputName "cat"
              , forceRight $ System.Nix.OutputName.mkOutputName "dog"
              ])
            (MonoidalMap.fromList
              [ ( forceRight $ System.Nix.OutputName.mkOutputName "cat"
                , ChildNode $ This $ Set.singleton $ forceRight $ System.Nix.OutputName.mkOutputName "kitten"
                )
              , ( forceRight $ System.Nix.OutputName.mkOutputName "goose"
                , ChildNode $ This $ Set.singleton $ forceRight $ System.Nix.OutputName.mkOutputName "gosling"
                )
              ])
          )
      }
  , platform = "wasm-sel4"
  , builder = "foo"
  , args = Vector.fromList ["bar", "baz"]
  , env = Map.fromList [("BIG_BAD", "WOLF")]
  }

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    describe "full derivations" $ do
      it "parses simple-derivation.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/simple-derivation.json"
        json <- BSL.readFile path
        eitherDecode json `shouldBe` Right simpleDerivation

      it "parses dyn-dep-derivation.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/dyn-dep-derivation.json"
        json <- BSL.readFile path
        eitherDecode json `shouldBe` Right dynDepDerivation

    describe "output types" $ do
      it "parses output-inputAddressed.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/output-inputAddressed.json"
        json <- BSL.readFile path
        output <- case eitherDecode json of
          Right output -> pure output
          Left err -> fail err

        let expectedPath = forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-drv-name-output-name"

        output `shouldBe` InputAddressedDerivationOutput expectedPath

      it "parses output-caFixedFlat.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/output-caFixedFlat.json"
        json <- BSL.readFile path
        output <- case eitherDecode json of
          Right output -> pure output
          Left err -> fail err

        let expectedHash = either error id $ decodeDigestWith @SHA256 Base64 "iUUXyRY8iW7DGirb0zwGgf1fRbLA7wimTJKgP7l/OQ8="

        output `shouldBe` FixedDerivationOutput ContentAddressMethod_Flat (HashAlgo_SHA256 :=> expectedHash)

      it "parses output-caFixedNAR.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/output-caFixedNAR.json"
        json <- BSL.readFile path
        output <- case eitherDecode json of
          Right output -> pure output
          Left err -> fail err

        let expectedHash = either error id $ decodeDigestWith @SHA256 Base64 "iUUXyRY8iW7DGirb0zwGgf1fRbLA7wimTJKgP7l/OQ8="

        output `shouldBe` FixedDerivationOutput ContentAddressMethod_NixArchive (HashAlgo_SHA256 :=> expectedHash)

      it "parses output-caFixedText.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/output-caFixedText.json"
        json <- BSL.readFile path
        output <- case eitherDecode json of
          Right output -> pure output
          Left err -> fail err

        let expectedHash = either error id $ decodeDigestWith @SHA256 Base64 "iUUXyRY8iW7DGirb0zwGgf1fRbLA7wimTJKgP7l/OQ8="

        output `shouldBe` FixedDerivationOutput ContentAddressMethod_Text (HashAlgo_SHA256 :=> expectedHash)

      it "parses output-caFloating.json" $ do
        path <- getDataFileName "upstream-libstore-data/derivation/output-caFloating.json"
        json <- BSL.readFile path
        output <- case eitherDecode json of
          Right output -> pure output
          Left err -> fail err

        output `shouldBe` ContentAddressedDerivationOutput ContentAddressMethod_NixArchive (Some HashAlgo_SHA256)
