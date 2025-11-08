{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module JSONSpec where

import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
import Test.Hspec (Expectation, Spec, describe)
import Test.Hspec.Nix (roundtrips)
import Test.Hspec.QuickCheck (prop)

import System.Nix.Arbitrary ()
import System.Nix.ContentAddress (ContentAddress)
import System.Nix.Derivation (Derivation, FreeformDerivationOutput)
import System.Nix.DerivedPath (DerivedPath, OutputsSpec, SingleDerivedPath)
import System.Nix.JSON ({-HashJSON-})
import System.Nix.OutputName (OutputName)
import System.Nix.Realisation (BuildTraceKey, Realisation)
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart)

roundtripsJSON
  :: ( Eq a
     , Show a
     , ToJSON a
     , FromJSON a
     )
  => a
  -> Expectation
roundtripsJSON = roundtrips encode eitherDecode

spec :: Spec
spec = do
  describe "roundtrips" $ do
    prop "StorePathName" $ roundtripsJSON @StorePathName
    prop "StorePathHashPart" $ roundtripsJSON @StorePathHashPart
    prop "StorePath" $ roundtripsJSON @StorePath
    -- prop "HashJSON" $ roundtripsJSON @HashJSON
    prop "ContentAddress" $ roundtripsJSON @ContentAddress
    prop "OutputsSpec" $ roundtripsJSON @OutputsSpec
    prop "SingleDerivedPath" $ roundtripsJSON @SingleDerivedPath
    prop "DerivedPath" $ roundtripsJSON @DerivedPath
    prop "FreeformDerivationOutput" $ roundtripsJSON @FreeformDerivationOutput
    prop "Derivation" $ roundtripsJSON @Derivation
    prop "BuildTraceKey OutputName" $ roundtripsJSON @(BuildTraceKey OutputName)
    prop "Signature" $ roundtripsJSON @Signature
    prop "Realisation" $ roundtripsJSON @Realisation
