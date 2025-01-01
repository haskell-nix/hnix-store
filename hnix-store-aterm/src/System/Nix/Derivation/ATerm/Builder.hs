{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering logic

module System.Nix.Derivation.ATerm.Builder
    ( -- * Builder
      buildDerivation
    , buildDerivationWith
    , buildDerivationOutput
    , buildDerivationInputs
    ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import System.Nix.Derivation
    ( Derivation'(..)
    , DerivationOutput(..)
    )
import System.Nix.Derivation.Traditional
import System.Nix.StorePath
import System.Nix.OutputName

import Data.Map qualified
import Data.Set qualified
import Data.Text qualified
import Data.Text.Lazy.Builder qualified
import Data.Vector qualified

-- | Render a derivation as a `Builder`
buildDerivation
    :: StoreDir
    -> Derivation' TraditionalDerivationInputs DerivationOutput
    -> Builder
buildDerivation sd =
    buildDerivationWith
        (buildDerivationInputs sd)
        (buildDerivationOutput sd)

-- | Render a derivation as a `Builder` using custom
-- renderer for storePaths, texts, outputNames and derivation inputs/outputs
buildDerivationWith
    :: (drvInputs -> Builder)
    -> (StorePathName -> OutputName -> drvOutput -> Builder)
    -> Derivation' drvInputs drvOutput
    -> Builder
buildDerivationWith drvInputs drvOutput (Derivation {..}) =
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
        <>  buildOutputName key
        <>  ","
        <>  drvOutput name key output
        <>  ")"

    keyValue1 (key, value) =
            "("
        <>  string key
        <>  ","
        <>  string value
        <>  ")"

-- | Render a @DerivationOutput@ as a `Builder` using custom
-- renderer for storePaths
buildDerivationOutput
    :: StoreDir
    -> StorePathName
    -> OutputName
    -> DerivationOutput
    -> Builder
buildDerivationOutput storeDir drvName outputName =
  ( \RawDerivationOutput {..} ->
        string rawPath
    <>  ","
    <>  string rawMethodHashAlgo
    <>  ","
    <>  string rawHash
  )
  . renderRawDerivationOutput storeDir drvName outputName

-- | Render a @DerivationInputs@ as a `Builder` using custom
-- renderer for storePaths and output names
buildDerivationInputs
    :: StoreDir
    -> TraditionalDerivationInputs
    -> Builder
buildDerivationInputs storeDir (TraditionalDerivationInputs {..}) =
        mapOf keyValue traditionalDrvs
    <>  ","
    <>  setOf (storePath storeDir) traditionalSrcs
  where
    keyValue (key, value) =
            "("
        <>  storePath storeDir key
        <>  ","
        <>  setOf buildOutputName value
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

string :: Text -> Builder
string = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show

buildOutputName :: OutputName -> Builder
buildOutputName = string . unStorePathName . unOutputName

storePath :: StoreDir -> StorePath -> Builder
storePath sd = string . storePathToText sd
