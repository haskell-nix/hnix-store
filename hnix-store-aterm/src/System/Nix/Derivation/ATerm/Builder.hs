{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering logic

module System.Nix.Derivation.ATerm.Builder
    ( -- * Builder
      buildTraditionalDerivation
    , buildTraditionalDerivationWith
    , buildFreeformDerivationOutput
    , buildTraditionalDerivationInputs
    ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import System.Nix.Derivation
    ( FreeformDerivationOutput(..)
    , FreeformDerivationOutputs
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
buildTraditionalDerivation
    :: StoreDir
    -> TraditionalDerivation' TraditionalDerivationInputs FreeformDerivationOutputs
    -> Builder
buildTraditionalDerivation sd =
    buildTraditionalDerivationWith
        (buildTraditionalDerivationInputs sd)
        (\_ -> buildFreeformDerivationOutput sd)

-- | Render a derivation as a `Builder` using custom
-- renderer for storePaths, texts, outputNames and derivation inputs/outputs
buildTraditionalDerivationWith
    :: (drvInputs -> Builder)
    -> (OutputName -> drvOutput -> Builder)
    -> TraditionalDerivation' drvInputs (Map OutputName drvOutput)
    -> Builder
buildTraditionalDerivationWith drvInputs drvOutput (TraditionalDerivation {..}) =
        "Derive("
    <>  mapOf keyValue0 anonOutputs
    <>  ","
    <>  drvInputs anonInputs
    <>  ","
    <>  string anonPlatform
    <>  ","
    <>  string anonBuilder
    <>  ","
    <>  vectorOf string anonArgs
    <>  ","
    <>  mapOf keyValue1 anonEnv
    <>  ")"
  where
    keyValue0 (key, output) =
            "("
        <>  buildOutputName key
        <>  ","
        <>  drvOutput key output
        <>  ")"

    keyValue1 (key, value) =
            "("
        <>  string key
        <>  ","
        <>  string value
        <>  ")"

-- | Render a @FreeformDerivationOutput@ as a `Builder` using custom
-- renderer for storePaths
buildFreeformDerivationOutput
    :: StoreDir
    -> FreeformDerivationOutput
    -> Builder
buildFreeformDerivationOutput storeDir =
  ( \RawDerivationOutput {..} ->
        string rawPath
    <>  ","
    <>  string rawMethodHashAlgo
    <>  ","
    <>  string rawHash
  )
  . renderRawDerivationOutput storeDir

-- | Render a @TraditionalDerivationInputs@ as a `Builder` using custom
-- renderer for storePaths and output names
buildTraditionalDerivationInputs
    :: StoreDir
    -> TraditionalDerivationInputs
    -> Builder
buildTraditionalDerivationInputs storeDir (TraditionalDerivationInputs {..}) =
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
string =
    Data.Text.Lazy.Builder.fromText
    . (\input -> Data.Text.concat ["\"", Data.Text.concatMap escapeChar input, "\""])
  where
    escapeChar :: Char -> Text
    escapeChar '\"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = Data.Text.singleton c

buildOutputName :: OutputName -> Builder
buildOutputName = string . unStorePathName . unOutputName

storePath :: StoreDir -> StorePath -> Builder
storePath sd = string . storePathToText sd
