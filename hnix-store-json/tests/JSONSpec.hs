{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module JSONSpec where

import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
import Test.Hspec (Expectation, Spec, describe)
import Test.Hspec.Nix (roundtrips)
import Test.Hspec.QuickCheck (prop)

import System.Nix.Arbitrary ()
import System.Nix.ContentAddress (ContentAddress)
import System.Nix.DerivedPath (DerivedPath, OutputsSpec, SingleDerivedPath)
import System.Nix.JSON ()
import System.Nix.Realisation (BuildTraceKey, Realisation)
import System.Nix.Signature (Signature, NamedSignature)
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
    prop "ContentAddress" $ roundtripsJSON @ContentAddress
    prop "OutputsSpec" $ roundtripsJSON @OutputsSpec
    prop "SingleDerivedPath" $ roundtripsJSON @SingleDerivedPath
    prop "DerivedPath" $ roundtripsJSON @DerivedPath
    prop "BuildTraceKey" $ roundtripsJSON @BuildTraceKey
    prop "Signature" $ roundtripsJSON @Signature
    prop "NamedSignature" $ roundtripsJSON @NamedSignature
    prop "Realisation" $ roundtripsJSON @Realisation
