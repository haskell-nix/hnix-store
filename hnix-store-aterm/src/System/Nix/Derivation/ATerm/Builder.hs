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

import Data.Dependent.Sum
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Set (Set)
import Data.Some
import Data.These (These(..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import System.Nix.ContentAddress
import System.Nix.Derivation
    ( Derivation
    , Derivation'(..)
    , DerivationOutput(..)
    , DerivationInputs(..)
    , DerivedPathMap(..)
    , unChildNode
    )
import System.Nix.Hash
import System.Nix.StorePath
import System.Nix.StorePath.ContentAddressed
import System.Nix.OutputName

import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector

-- | Render a derivation as a `Builder`
buildDerivation
    :: StoreDir
    -> Derivation
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

buildMethodHashAlgo :: ContentAddressMethod -> Some HashAlgo -> Text
buildMethodHashAlgo method hashAlgo = Data.Text.intercalate ":" $
  (case method of
    ContentAddressMethod_NixArchive -> ["r"]
    ContentAddressMethod_Text -> ["text"]
    ContentAddressMethod_Flat -> [])
  <>
  [withSome hashAlgo algoToText]

-- | Render a @DerivationOutput@ as a `Builder` using custom
-- renderer for storePaths
buildDerivationOutput
    :: StoreDir
    -> StorePathName
    -> OutputName
    -> DerivationOutput
    -> Builder
buildDerivationOutput storeDir drvName outputName = \case
  InputAddressedDerivationOutput {..} ->
        storePath storeDir path
    <>  ","
    <>  emptyString
    <>  ","
    <>  emptyString
  FixedDerivationOutput {..} -> case hash of
    hashAlgo :=> hash' ->
          storePath storeDir (makeFixedOutputPath storeDir method hash mempty $ outputStoreObjectName drvName outputName)
      <>  ","
      <>  string (buildMethodHashAlgo method $ Some hashAlgo)
      <>  ","
      <>  string (encodeDigestWith NixBase32 hash')
  ContentAddressedDerivationOutput {..} ->
        emptyString
    <>  ","
    <>  string (buildMethodHashAlgo method hashAlgo)
    <>  ","
    <>  emptyString
  where
    emptyString = string Data.Text.empty

-- | Render a @DerivationInputs@ as a `Builder` using custom
-- renderer for storePaths and output names
buildDerivationInputs
    :: StoreDir
    -> DerivationInputs
    -> Builder
buildDerivationInputs storeDir (DerivationInputs {..}) =
        mapOf keyValue (getMonoidalMap $ unDerivedPathMap drvs)
    <>  ","
    <>  setOf (storePath storeDir) srcs
  where
    keyValue (key, value) =
            "("
        <>  storePath storeDir key
        <>  ","
        <>  setOf buildOutputName outputNames
        <>  ")"
     where outputNames = case unChildNode value of
             This os -> os
             _ -> error "dynamic derivations are not supported in the ATerm format yet"

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
