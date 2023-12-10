{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering logic

module Nix.Derivation.Builder
    ( -- * Builder
      buildDerivation
    , buildDerivationWith
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
           DerivationOutput
           DerivationInputs
    -> Builder
buildDerivation =
    buildDerivationWith
        string'
        string'
        (buildDerivationOutputWith filepath')
        (buildDerivationInputsWith filepath' string')

-- | Render a derivation as a `Builder` using custom
-- renderer for filepaths, texts, outputNames and derivation inputs/outputs
buildDerivationWith
    :: (txt -> Builder)
    -> (outputName -> Builder)
    -> (drvOutput fp -> Builder)
    -> (drvInputs fp outputName -> Builder)
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
    :: Monoid fp
    => (fp -> Builder)
    -> DerivationOutput fp
    -> Builder
buildDerivationOutputWith filepath (DerivationOutput {..}) =
        filepath path
    <>  ","
    <>  string' mempty
    <>  ","
    <>  string' mempty
buildDerivationOutputWith filepath (FixedDerivationOutput {..}) =
        filepath path
    <>  ","
    <>  string' hashAlgo
    <>  ","
    <>  string' hash
buildDerivationOutputWith filepath (ContentAddressedDerivationOutput {..}) =
        filepath mempty
    <>  ","
    <>  string' hashAlgo
    <>  ","
    <>  string' mempty

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
