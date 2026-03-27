{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Parsing logic

module Nix.Derivation.Parser
    ( -- * Parser
      parseDerivation
    , parseDerivationWith
    , textParser
    ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Nix.Derivation.Types (Derivation(..), DerivationOutput(..))

import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Vector
import qualified System.FilePath

listOf :: Parser a -> Parser [a]
listOf element = do
    "["
    es <- Data.Attoparsec.Text.Lazy.sepBy element ","
    "]"
    return es

-- | Parse a derivation
parseDerivation :: Parser (Derivation FilePath Text)
parseDerivation = parseDerivationWith filepathParser textParser

-- | Parse a derivation using custom
-- parsers for filepaths and text fields
parseDerivationWith :: (Ord fp, Ord txt) => Parser fp -> Parser txt -> Parser (Derivation fp txt)
parseDerivationWith filepath string = do
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

textParser :: Parser Text
textParser = do
    "\""

    let predicate c = not (c == '"' || c == '\\')

    let loop = do
            text0 <- Data.Attoparsec.Text.takeWhile predicate

            char0 <- Data.Attoparsec.Text.anyChar

            case char0 of
                '"'  -> do
                    return [ text0 ]

                _    -> do
                    char1 <- Data.Attoparsec.Text.anyChar

                    char2 <- case char1 of
                        'n' -> return '\n'
                        'r' -> return '\r'
                        't' -> return '\t'
                        _   -> return char1

                    textChunks <- loop

                    return (text0 : Data.Text.singleton char2 : textChunks)

    textChunks <- loop

    return (Data.Text.concat textChunks)

filepathParser :: Parser FilePath
filepathParser = do
    text <- textParser
    let str = Data.Text.unpack text
    case (Data.Text.uncons text, System.FilePath.isValid str) of
        (Just ('/', _), True) -> do
            return str
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
