{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Parsing logic

module Nix.Derivation.Parser
    ( -- * Parser
      parseDerivation
    , parseDerivationWith
    , parseDerivationOutputWith
    , parseDerivationInputsWith
    , textParser
    ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Nix.Derivation.Types
    ( Derivation(..)
    , DerivationInputs(..)
    , DerivationOutput(..)
    )

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
    pure es

-- | Parse a derivation
parseDerivation
    :: Parser (Derivation
                  FilePath
                  Text
                  Text
                  (DerivationOutput FilePath Text)
                  (DerivationInputs FilePath Text)
              )
parseDerivation =
    parseDerivationWith
        textParser
        textParser
        (parseDerivationOutputWith filepathParser textParser)
        (parseDerivationInputsWith filepathParser textParser)

-- | Parse a derivation using custom
-- parsers for filepaths, texts, outputNames and derivation inputs/outputs
parseDerivationWith
    :: ( Ord fp
       , Ord txt
       , Ord outputName
       )
    => Parser txt
    -> Parser outputName
    -> Parser drvOutput
    -> Parser drvInputs
    -> Parser (Derivation fp txt outputName drvOutput drvInputs)
parseDerivationWith string outputName parseOutput parseInputs = do
    "Derive("

    let keyValue0 = do
            "("
            key <- outputName
            ","
            drvOutput <- parseOutput
            ")"
            return (key, drvOutput)
    outputs <- mapOf keyValue0

    ","

    inputs <- parseInputs

    ","

    platform <- string

    ","

    builder <- string

    ","

    args <- vectorOf string

    ","

    let keyValue1 = do
            "("
            key <- string
            ","
            value <- string
            ")"
            pure (key, value)
    env <- mapOf keyValue1

    ")"

    pure Derivation {..}

-- | Parse a derivation output
parseDerivationOutputWith
    :: Parser fp
    -> Parser txt
    -> Parser (DerivationOutput fp txt)
parseDerivationOutputWith filepath textParser = do
    mPath <- maybeTextParser filepath
    ","
    mHashAlgo <- maybeTextParser textParser
    ","
    mHash <- maybeTextParser textParser
    case (mPath, mHashAlgo, mHash) of
        (Just path, Nothing, Nothing) ->
              pure InputAddressedDerivationOutput {..}
        (Just path, Just hashAlgo, Just hash) ->
              pure FixedDerivationOutput {..}
        (Nothing, Just hashAlgo, Nothing) ->
              pure ContentAddressedDerivationOutput {..}
        _ ->
            fail "bad output in derivation"

-- | Parse a derivation inputs
parseDerivationInputsWith
    :: ( Ord fp
       , Ord outputName
       )
    => Parser fp
    -> Parser outputName
    -> Parser (DerivationInputs fp outputName)
parseDerivationInputsWith filepath outputName = do
    let keyValue = do
            "("
            key <- filepath
            ","
            value <- setOf outputName
            ")"
            pure (key, value)
    drvs <- mapOf keyValue

    ","

    srcs <- setOf filepath
    pure DerivationInputs {..}

maybeTextParser :: Parser a -> Parser (Maybe a)
maybeTextParser p = do
  t <- textParser
  if t == ""
    then pure Nothing
    else Just <$> p

textParser :: Parser Text
textParser = do
    "\""

    let predicate c = not (c == '"' || c == '\\')

    let loop = do
            text0 <- Data.Attoparsec.Text.takeWhile predicate

            char0 <- Data.Attoparsec.Text.anyChar

            case char0 of
                '"'  -> do
                    pure [ text0 ]

                _    -> do
                    char1 <- Data.Attoparsec.Text.anyChar

                    char2 <- case char1 of
                        'n' -> pure '\n'
                        'r' -> pure '\r'
                        't' -> pure '\t'
                        _   -> pure char1

                    textChunks <- loop

                    pure (text0 : Data.Text.singleton char2 : textChunks)

    Data.Text.concat <$> loop

filepathParser :: Parser FilePath
filepathParser = do
    text <- textParser
    let str = Data.Text.unpack text
    case (Data.Text.uncons text, System.FilePath.isValid str) of
        (Just ('/', _), True) -> do
            pure str
        _ -> do
            fail ("bad path ‘" <> Data.Text.unpack text <> "’ in derivation")

setOf :: Ord a => Parser a -> Parser (Set a)
setOf element = do
    es <- listOf element
    pure (Data.Set.fromList es)

vectorOf :: Parser a -> Parser (Vector a)
vectorOf element = do
    es <- listOf element
    pure (Data.Vector.fromList es)

mapOf :: Ord k => Parser (k, v) -> Parser (Map k v)
mapOf keyValue = do
    keyValues <- listOf keyValue
    pure (Data.Map.fromList keyValues)
