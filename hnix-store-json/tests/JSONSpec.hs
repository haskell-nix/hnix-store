{-# LANGUAGE OverloadedStrings #-}
module JSONSpec where

import Data.Aeson (ToJSON, FromJSON, decode, encode)
import Data.Default.Class (Default(def))
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()
import System.Nix.JSON ()
import System.Nix.OutputName (OutputName)
import System.Nix.Realisation (DerivationOutput(..), Realisation(..))
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart)

import qualified Data.Map
import qualified Data.Set
import qualified System.Nix.Hash
import qualified System.Nix.OutputName
import qualified System.Nix.Signature
import qualified System.Nix.StorePath

roundtripsJSON
  :: ( Eq a
     , Show a
     , ToJSON a
     , FromJSON a
     )
  => a
  -> Expectation
roundtripsJSON = roundtrips encode decode

sampleDerivationOutput :: DerivationOutput OutputName
sampleDerivationOutput = DerivationOutput
  { derivationOutputHash =
      forceRight
      $ System.Nix.Hash.mkNamedDigest
          "sha256"
          "1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0"
  , derivationOutputName =
      forceRight
      $ System.Nix.OutputName.mkOutputName "foo"
  }

sampleRealisation0 :: Realisation
sampleRealisation0 = Realisation
  { realisationOutPath =
      forceRight
      $ System.Nix.StorePath.parsePath
          def
          "/nix/store/cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh"
  , realisationSignatures = mempty
  , realisationDependencies = mempty
  }

sampleRealisation1 :: Realisation
sampleRealisation1 = Realisation
  { realisationOutPath =
      forceRight
      $ System.Nix.StorePath.parsePath
          def
          "/nix/store/5rwxzi7pal3qhpsyfc16gzkh939q1np6-curl-7.82.0.drv"
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
          $ System.Nix.StorePath.parsePathFromText
              def
              "/nix/store/9472ijanf79nlkb5n1yh57s7867p1930-testFixed"
        )
      ]
  }

spec :: Spec
spec = do
  describe "JSON" $ do
    describe "roundtrips" $ do
      prop "StorePathName" $ roundtripsJSON @StorePathName
      prop "StorePathHashPart" $ roundtripsJSON @StorePathHashPart
      prop "StorePath" $ roundtripsJSON @StorePath
      prop "DerivationOutput OutputName" $ roundtripsJSON @(DerivationOutput OutputName)
      prop "Signature" $ roundtripsJSON @Signature
      prop "Realisation" $ roundtripsJSON @Realisation

    describe "ground truth" $ do
      it "sampleDerivationOutput matches preimage" $
        encode sampleDerivationOutput `shouldBe` "\"sha256:1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0!foo\""

      it "sampleRealisation0 matches preimage" $
        encode sampleRealisation0 `shouldBe` "{\"outPath\":\"cdips4lakfk1qbf1x68fq18wnn3r5r14-builder.sh\",\"signatures\":[],\"dependentRealisations\":{}}"

      it "sampleRealisation1 matches preimage" $
        encode sampleRealisation1 `shouldBe` "{\"outPath\":\"5rwxzi7pal3qhpsyfc16gzkh939q1np6-curl-7.82.0.drv\",\"signatures\":[\"SMjnB3mPgXYjXacU+xN24BdzXlAgGAuFnYwPddU3bhjfHBeQus/OimdIPMgR/JMKFPHXORrk7pbjv68vecTEBA==\",\"fW3iEMfyx6IZzGNswD54BjclfkXiYzh0xRXddrXfJ1rp1l8p1xTi9/0g2EibbwLFb6p83cwIJv5KtTGksC54CQ==\"],\"dependentRealisations\":{\"sha256:1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0!foo\":\"9472ijanf79nlkb5n1yh57s7867p1930-testFixed\"}}"

forceRight
  :: Show a
  => Either a b
  -> b
forceRight = \case
  Right x -> x
  Left e -> error $ "fromRight failed: " ++ show e
