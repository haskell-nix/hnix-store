{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering logic

module Nix.Derivation.Builder
    ( -- * Builder
      buildDerivation
    , buildDerivationWith
    , buildDerivationOutputWith
    , buildDerivationInputsWith
    ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Nix.Derivation.Types
    ( Derivation(..)
    , DerivationInputs(..)
    , DerivationOutput(..)
    )

import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector

-- | Render a derivation as a `Builder`
buildDerivation
    :: Derivation
           FilePath
           Text
           Text
           (DerivationOutput FilePath Text)
           (DerivationInputs FilePath Text)
    -> Builder
buildDerivation =
    buildDerivationWith
        string'
        string'
        (buildDerivationOutputWith filepath' string' emptyString')
        (buildDerivationInputsWith filepath' string')
  where
    emptyString' = string' Data.Text.empty

-- | Render a derivation as a `Builder` using custom
-- renderer for filepaths, texts, outputNames and derivation inputs/outputs
buildDerivationWith
    :: (txt -> Builder)
    -> (outputName -> Builder)
    -> (drvOutput -> Builder)
    -> (drvInputs -> Builder)
    -> Derivation fp txt outputName drvOutput drvInputs
    -> Builder
buildDerivationWith string outputName drvOutput drvInputs (Derivation {..}) =
        "Derive("
    <>  mapOf keyValue0 outputs
    <>  ","
    <>  drvInputs inputs
    <>  ","
    <>  string platform
    <>  ","
    <>  string builder
    <>  ","
    <>  vectorOf string args
    <>  ","
    <>  mapOf keyValue1 env
    <>  ")"
  where
    keyValue0 (key, output) =
            "("
        <>  outputName key
        <>  ","
        <>  drvOutput output
        <>  ")"

    keyValue1 (key, value) =
            "("
        <>  string key
        <>  ","
        <>  string value
        <>  ")"

-- | Render a @DerivationOutput@ as a `Builder` using custom
-- renderer for filepaths
buildDerivationOutputWith
    :: (fp -> Builder)
    -> (txt -> Builder)
    -> Builder
    -> DerivationOutput fp txt
    -> Builder
buildDerivationOutputWith filepath string emptyString = \case
  InputAddressedDerivationOutput {..} ->
        filepath path
    <>  ","
    <>  emptyString
    <>  ","
    <>  emptyString
  FixedDerivationOutput {..} ->
        filepath path
    <>  ","
    <>  string hashAlgo
    <>  ","
    <>  string hash
  ContentAddressedDerivationOutput {..} ->
        emptyString
    <>  ","
    <>  string hashAlgo
    <>  ","
    <>  emptyString

-- | Render a @DerivationInputs@ as a `Builder` using custom
-- renderer for filepaths and output names
buildDerivationInputsWith
    :: (fp -> Builder)
    -> (outputName -> Builder)
    -> DerivationInputs fp outputName
    -> Builder
buildDerivationInputsWith filepath outputName (DerivationInputs {..}) =
        mapOf keyValue drvs
    <>  ","
    <>  setOf filepath srcs
  where
    keyValue (key, value) =
            "("
        <>  filepath key
        <>  ","
        <>  setOf outputName value
        <>  ")"

mapOf :: ((k, v) -> Builder) -> Map k v -> Builder
mapOf keyValue m = listOf keyValue (Data.Map.toList m)

listOf :: (a -> Builder) -> [a] -> Builder
listOf _          []  = "[]"
listOf element (x:xs) =
        "["
    <>  element x
    <>  foldMap rest xs
    <>  "]"
  where
    rest y = "," <> element y

setOf :: (a -> Builder) -> Set a -> Builder
setOf element xs = listOf element (Data.Set.toList xs)

vectorOf :: (a -> Builder) -> Vector a -> Builder
vectorOf element xs = listOf element (Data.Vector.toList xs)

string' :: Text -> Builder
string' = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show

filepath' :: FilePath -> Builder
filepath' p = string' $ Data.Text.pack p
