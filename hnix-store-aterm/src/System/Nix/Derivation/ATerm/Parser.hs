{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Parsing logic

module System.Nix.Derivation.ATerm.Parser
    ( -- * Parser
      parseDerivation
    , parseDerivationWith
    , parseDerivationOutput
    , parseDerivationInputs
    , textParser
    ) where

import Data.Attoparsec.Text qualified
import Data.Attoparsec.Text.Lazy (Parser)
import Data.Attoparsec.Text.Lazy qualified
import Data.Map (Map)
import Data.Map qualified
import Data.Set (Set)
import Data.Set qualified
import Data.Text (Text)
import Data.Text qualified
import Data.Vector (Vector)
import Data.Vector qualified

import System.Nix.Derivation.Traditional
import System.Nix.Derivation
    ( Derivation'(..)
    , DerivationOutput(..)
    )
import System.Nix.StorePath
import System.Nix.OutputName

listOf :: Parser a -> Parser [a]
listOf element = do
    "["
    es <- Data.Attoparsec.Text.Lazy.sepBy element ","
    "]"
    pure es

-- | Parse a derivation
parseDerivation :: StoreDir -> StorePathName -> Parser (Derivation' TraditionalDerivationInputs DerivationOutput)
parseDerivation sd =
    parseDerivationWith
        (parseDerivationInputs sd)
        (parseDerivationOutput sd)

-- | Parse a derivation using custom
-- parsers for filepaths, texts, outputNames and derivation inputs/outputs
parseDerivationWith
    :: Parser drvInputs
    -> (StorePathName -> OutputName -> Parser drvOutput)
    -> StorePathName
    -> Parser (Derivation' drvInputs drvOutput)
parseDerivationWith parseInputs parseOutput name = do
    "Derive("

    let keyValue0 = do
            "("
            key <- outputNameParser
            ","
            drvOutput <- parseOutput name key
            ")"
            return (key, drvOutput)
    outputs <- mapOf keyValue0

    ","

    inputs <- parseInputs

    ","

    platform <- textParser

    ","

    builder <- textParser

    ","

    args <- vectorOf textParser

    ","

    let keyValue1 = do
            "("
            key <- textParser
            ","
            value <- textParser
            ")"
            pure (key, value)
    env <- mapOf keyValue1

    ")"

    pure Derivation {..}

-- | Parse a derivation output
parseDerivationOutput :: StoreDir -> StorePathName -> OutputName -> Parser DerivationOutput
parseDerivationOutput sd drvName outputName = do
    rawPath <- textParser
    ","
    rawMethodHashAlgo <- textParser
    ","
    rawHash <- textParser
    parseRawDerivationOutput sd drvName outputName $ RawDerivationOutput {..}

-- | Parse a derivation inputs
parseDerivationInputs :: StoreDir -> Parser TraditionalDerivationInputs
parseDerivationInputs sd = do
    traditionalDrvs <- mapOf $ do
         "("
         key <- storePathParser sd
         ","
         value <- setOf outputNameParser
         ")"
         pure (key, value)

    ","

    traditionalSrcs <- setOf $ storePathParser sd
    pure TraditionalDerivationInputs {..}

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

outputNameParser :: Parser OutputName
outputNameParser = do
  n <- textParser
  case mkOutputName n of
    Left e -> fail $ show e -- TODO
    Right sp -> pure sp

storePathParser :: StoreDir -> Parser StorePath
storePathParser sd = do
  f <- textParser
  case System.Nix.StorePath.parsePathFromText sd f of
    Left e -> fail $ show e -- TODO
    Right sp -> pure sp

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
