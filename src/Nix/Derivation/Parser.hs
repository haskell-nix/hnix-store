{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Parsing logic

module Nix.Derivation.Parser
    ( -- * Parser
      parseDerivation
    ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Nix.Derivation.Types (Derivation(..), DerivationOutput(..))
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector
import qualified Filesystem.Path.CurrentOS

listOf :: Parser a -> Parser [a]
listOf element = do
    "["
    es <- Data.Attoparsec.Text.Lazy.sepBy element ","
    "]"
    return es

-- | Parse a derivation
parseDerivation :: Parser Derivation
parseDerivation = do
    "Derive("

    let keyValue0 = do
            "("
            key <- string
            ","
            path <- filepath
            ","
            hashAlgo <- string
            ","
            hash <- string
            ")"
            return (key, DerivationOutput {..})
    outputs <- mapOf keyValue0

    ","

    let keyValue1 = do
            "("
            key <- filepath
            ","
            value <- setOf string
            ")"
            return (key, value)
    inputDrvs <- mapOf keyValue1

    ","

    inputSrcs <- setOf filepath

    ","

    platform <- string

    ","

    builder <- string

    ","

    args <- vectorOf string

    ","

    let keyValue2 = do
            "("
            key <- string
            ","
            value <- string
            ")"
            return (key, value)
    env <- mapOf keyValue2

    ")"

    return (Derivation {..})

string :: Parser Text
string = do
    "\""
    let predicate c = not (c == '"' || c == '\\')
    let loop = do
            text0 <- Data.Attoparsec.Text.Lazy.takeWhile predicate
            char0 <- Data.Attoparsec.Text.Lazy.anyChar
            text2 <- case char0 of
                '"'  -> return ""
                _    -> do
                    char1 <- Data.Attoparsec.Text.Lazy.anyChar
                    char2 <- case char1 of
                        'n' -> return '\n'
                        'r' -> return '\r'
                        't' -> return '\t'
                        _   -> return char1
                    text1 <- loop
                    return (Data.Text.Lazy.cons char2 text1)
            return (Data.Text.Lazy.fromStrict text0 <> text2)
    text <- loop
    return (Data.Text.Lazy.toStrict text)

filepath :: Parser FilePath
filepath = do
    text <- string
    case Data.Text.uncons text of
        Just ('/', _) -> do
            return (Filesystem.Path.CurrentOS.fromText text)
        _ -> do
            fail ("bad path ‘" <> Data.Text.unpack text <> "’ in derivation")

setOf :: Ord a => Parser a -> Parser (Set a)
setOf element = do
    es <- listOf element
    return (Data.Set.fromList es)

vectorOf :: Parser a -> Parser (Vector a)
vectorOf element = do
    es <- listOf element
    return (Data.Vector.fromList es)

mapOf :: Ord k => Parser (k, v) -> Parser (Map k v)
mapOf keyValue = do
    keyValues <- listOf keyValue
    return (Data.Map.fromList keyValues)
