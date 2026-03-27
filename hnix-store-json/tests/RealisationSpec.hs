{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RealisationSpec where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.Realisation (BuildTraceKey(..), Realisation(..), RealisationWithId(..))
import System.Nix.StorePath qualified

sampleRealisation0 :: Realisation
sampleRealisation0 = Realisation
  { realisationOutPath =
      forceRight
      $ System.Nix.StorePath.parseBasePath
          "cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh"
  , realisationSignatures = mempty
  }

-- upstream-nix/src/libstore-tests/data/realisation/simple.json
upstreamSimpleRealisation :: RealisationWithId
upstreamSimpleRealisation = RealisationWithId
  ( BuildTraceKey
      { buildTraceKeyDrvPath =
          forceRight
          $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar.drv"
      , buildTraceKeyOutput =
          forceRight
          $ System.Nix.OutputName.mkOutputName "foo"
      }
  , Realisation
      { realisationOutPath =
          forceRight
          $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo"
      , realisationSignatures = mempty
      }
  )

-- upstream-nix/src/libstore-tests/data/realisation/unkeyed-simple.json
upstreamUnkeyedSimple :: Realisation
upstreamUnkeyedSimple = Realisation
  { realisationOutPath =
      forceRight
      $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo"
  , realisationSignatures = mempty
  }

spec :: Spec
spec = do
  describe "ground truth" $ do
    it "sampleRealisation0 matches preimage (relative paths)" $
      encode sampleRealisation0 `shouldBe` "{\"outPath\":\"cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh\",\"signatures\":[]}"

  let dir = "upstream-libstore-data/realisation"
  describe "upstream Nix test data" $ do
    parsesUpstream dir "simple.json" upstreamSimpleRealisation
    parsesUpstream dir "unkeyed-simple.json" upstreamUnkeyedSimple

    it "parses with-signature.json" $ do
      path <- getDataFileName "upstream-libstore-data/realisation/with-signature.json"
      json <- BSL.readFile path
      let result = eitherDecode json :: Either String RealisationWithId
      result `shouldSatisfy` \case
        Right _ -> True
        Left _ -> False

    it "parses unkeyed-with-signature.json" $ do
      path <- getDataFileName "upstream-libstore-data/realisation/unkeyed-with-signature.json"
      json <- BSL.readFile path
      let result = eitherDecode json :: Either String Realisation
      result `shouldSatisfy` \case
        Right _ -> True
        Left _ -> False
