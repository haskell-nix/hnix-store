{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Nix.Derivation.Builder where

import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Filesystem.Path.CurrentOS (FilePath)
import Nix.Derivation.Types (Derivation(..), DerivationOutput(..))
import Prelude hiding (FilePath)

import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Filesystem.Path.CurrentOS

buildDerivation :: Derivation -> Builder
buildDerivation (Derivation {..}) =
        "Derive("
    <>  mapOf keyValue0 outputs
    <>  ","
    <>  mapOf keyValue1 inputDrvs
    <>  ","
    <>  setOf filepath inputSrcs
    <>  ","
    <>  string platform
    <>  ","
    <>  filepath builder
    <>  ","
    <>  vectorOf string args
    <>  ","
    <>  mapOf keyValue2 env
    <>  ")"
  where
    keyValue0 (key, DerivationOutput {..}) =
            "("
        <>  string key
        <>  ","
        <>  filepath path
        <>  ","
        <>  string hashAlgo
        <>  ","
        <>  string hash
        <>  ")"

    keyValue1 (key, value) =
            "("
        <>  filepath key
        <>  ","
        <>  setOf string value
        <>  ")"

    keyValue2 (key, value) =
            "("
        <>  string key
        <>  ","
        <>  string value
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

filepath :: FilePath -> Builder
filepath p = string text
  where
    text = case Filesystem.Path.CurrentOS.toText p of
        Left  t -> t
        Right t -> t
