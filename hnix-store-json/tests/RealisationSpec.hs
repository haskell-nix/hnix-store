{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RealisationSpec where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (isInfixOf)
import Data.Map qualified
import Data.Set qualified
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Nix (forceRight)

import System.Nix.Hash qualified
import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.Realisation (DerivationOutput(..), Realisation(..), RealisationWithId(..))
import System.Nix.Signature qualified
import System.Nix.StorePath qualified

sampleDerivationOutput :: DerivationOutput System.Nix.OutputName.OutputName
sampleDerivationOutput = DerivationOutput
  { derivationOutputHash =
      forceRight
      $ System.Nix.Hash.mkNamedDigest
          "sha256"
          "1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0"
  , derivationOutputOutput =
      forceRight
      $ System.Nix.OutputName.mkOutputName "foo"
  }

sampleRealisation0 :: Realisation
sampleRealisation0 = Realisation
  { realisationOutPath =
      forceRight
      $ System.Nix.StorePath.parseBasePath
          "cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh"
  , realisationSignatures = mempty
  , realisationDependencies = mempty
  }

sampleRealisation1 :: Realisation
sampleRealisation1 = Realisation
  { realisationOutPath =
      forceRight
      $ System.Nix.StorePath.parseBasePath
          "5rwxzi7pal3qhpsyfc16gzkh939q1np6-curl-7.82.0.drv"
  , realisationSignatures =
      Data.Set.fromList
      $ forceRight
      . System.Nix.Signature.parseSignature
      <$> [ "fW3iEMfyx6IZzGNswD54BjclfkXiYzh0xRXddrXfJ1rp1l8p1xTi9/0g2EibbwLFb6p83cwIJv5KtTGksC54CQ=="
          , "SMjnB3mPgXYjXacU+xN24BdzXlAgGAuFnYwPddU3bhjfHBeQus/OimdIPMgR/JMKFPHXORrk7pbjv68vecTEBA=="
          ]
  , realisationDependencies =
      Data.Map.fromList
      [ ( sampleDerivationOutput
        , forceRight
          $ System.Nix.StorePath.parseBasePathFromText
              "9472ijanf79nlkb5n1yh57s7867p1930-testFixed"
        )
      ]
  }

-- upstream-nix/src/libstore-tests/data/realisation/simple.json
upstreamSimpleRealisation :: RealisationWithId
upstreamSimpleRealisation = RealisationWithId
  ( DerivationOutput
      { derivationOutputHash =
          forceRight
          $ System.Nix.Hash.mkNamedDigest
              "sha256"
              "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      , derivationOutputOutput =
          forceRight
          $ System.Nix.OutputName.mkOutputName "foo"
      }
  , Realisation
      { realisationOutPath =
          forceRight
          $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"
      , realisationSignatures = mempty
      , realisationDependencies = mempty
      }
  )

-- upstream-nix/src/libstore-tests/data/realisation/with-dependent-realisations.json
upstreamWithDependentRealisations :: RealisationWithId
upstreamWithDependentRealisations = RealisationWithId
  ( DerivationOutput
      { derivationOutputHash =
          forceRight
          $ System.Nix.Hash.mkNamedDigest
              "sha256"
              "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      , derivationOutputOutput =
          forceRight
          $ System.Nix.OutputName.mkOutputName "foo"
      }
  , Realisation
      { realisationOutPath =
          forceRight
          $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"
      , realisationSignatures = mempty
      , realisationDependencies =
          Data.Map.fromList
          [ ( DerivationOutput
                { derivationOutputHash =
                    forceRight
                    $ System.Nix.Hash.mkNamedDigest
                        "sha256"
                        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                , derivationOutputOutput =
                    forceRight
                    $ System.Nix.OutputName.mkOutputName "foo"
                }
            , forceRight
              $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"
            )
          ]
      }
  )

spec :: Spec
spec = do
  describe "ground truth" $ do
    it "sampleDerivationOutput matches preimage" $
      encode sampleDerivationOutput `shouldBe` "\"sha256:1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0!foo\""

    it "sampleRealisation0 matches preimage (relative paths)" $
      encode sampleRealisation0 `shouldBe` "{\"outPath\":\"cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh\",\"signatures\":[],\"dependentRealisations\":{}}"

    it "sampleRealisation1 matches preimage (relative paths)" $
      encode sampleRealisation1 `shouldBe` "{\"outPath\":\"5rwxzi7pal3qhpsyfc16gzkh939q1np6-curl-7.82.0.drv\",\"signatures\":[\"SMjnB3mPgXYjXacU+xN24BdzXlAgGAuFnYwPddU3bhjfHBeQus/OimdIPMgR/JMKFPHXORrk7pbjv68vecTEBA==\",\"fW3iEMfyx6IZzGNswD54BjclfkXiYzh0xRXddrXfJ1rp1l8p1xTi9/0g2EibbwLFb6p83cwIJv5KtTGksC54CQ==\"],\"dependentRealisations\":{\"sha256:1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0!foo\":\"9472ijanf79nlkb5n1yh57s7867p1930-testFixed\"}}"

  describe "upstream Nix test data" $ do
    it "parses simple.json with relative store paths" $ do
      path <- getDataFileName "upstream-libstore-data/realisation/simple.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamSimpleRealisation

    it "parses with-dependent-realisations.json with relative store paths" $ do
      path <- getDataFileName "upstream-libstore-data/realisation/with-dependent-realisations.json"
      json <- BSL.readFile path
      eitherDecode json `shouldBe` Right upstreamWithDependentRealisations

    it "attempts to parse with-signature.json (fails due to invalid signature in test data)" $ do
      path <- getDataFileName "upstream-libstore-data/realisation/with-signature.json"
      json <- BSL.readFile path
      -- The upstream test file contains an invalid signature "asdfasdfasdf"
      -- which fails our signature validation
      eitherDecode json `shouldSatisfy` \case
        Left err -> "CryptoError" `isInfixOf` (err :: String)
        Right (_ :: RealisationWithId) -> False
