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
import Nix.Derivation.Types (Derivation(..), DerivationOutput(..))

import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector

-- | Render a derivation as a `Builder`
buildDerivation :: Derivation FilePath Text -> Builder
buildDerivation = buildDerivationWith filepath' string'

-- | Render a derivation as a `Builder` using custom
-- renderer for filepath and string
buildDerivationWith :: (Monoid fp, Monoid txt)
                    => (fp -> Builder)
                    -> (txt -> Builder)
                    -> Derivation fp txt
                    -> Builder
buildDerivationWith filepath string (Derivation {..}) =
        "Derive("
    <>  mapOf keyValue0 outputs
    <>  ","
    <>  mapOf keyValue1 inputDrvs
    <>  ","
    <>  setOf filepath inputSrcs
    <>  ","
    <>  string platform
    <>  ","
    <>  string builder
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
        <>  string mempty
        <>  ","
        <>  string mempty
        <>  ")"

    keyValue0 (key, FixedDerivationOutput {..}) =
            "("
        <>  string key
        <>  ","
        <>  filepath path
        <>  ","
        <>  string hashAlgo
        <>  ","
        <>  string hash
        <>  ")"

    keyValue0 (key, ContentAddressedDerivationOutput {..}) =
            "("
        <>  string key
        <>  ","
        <>  filepath mempty
        <>  ","
        <>  string hashAlgo
        <>  ","
        <>  string mempty
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

string' :: Text -> Builder
string' = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show

filepath' :: FilePath -> Builder
filepath' p = string' $ Data.Text.pack p
