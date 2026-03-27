{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Parsing logic

module System.Nix.Derivation.ATerm.Parser
    ( -- * Parser
      parseTraditionalDerivation
    , parseTraditionalDerivationWith
    , parseFreeformDerivationOutput
    , parseTraditionalDerivationInputs
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

import System.Nix.Derivation
    ( FreeformDerivationOutput(..)
    , FreeformDerivationOutputs
    )
import System.Nix.Derivation.Traditional
import System.Nix.StorePath
import System.Nix.OutputName

listOf :: Parser a -> Parser [a]
listOf element = do
    "["
    es <- Data.Attoparsec.Text.Lazy.sepBy element ","
    "]"
    pure es

-- | Parse a derivation
parseTraditionalDerivation
  :: StoreDir
  -> Parser (TraditionalDerivation' TraditionalDerivationInputs FreeformDerivationOutputs)
parseTraditionalDerivation sd =
    parseTraditionalDerivationWith
        (parseTraditionalDerivationInputs sd)
        (\_ -> parseFreeformDerivationOutput sd)

-- | Parse a derivation using custom
-- parsers for filepaths, texts, outputNames and derivation inputs/outputs
parseTraditionalDerivationWith
    :: Parser drvInputs
    -> (OutputName -> Parser drvOutput)
    -> Parser (TraditionalDerivation' drvInputs (Map OutputName drvOutput))
parseTraditionalDerivationWith parseInputs parseOutput = do
    "Derive("

    let keyValue0 = do
            "("
            key <- outputNameParser
            ","
            drvOutput <- parseOutput key
            ")"
            return (key, drvOutput)
    anonOutputs <- mapOf keyValue0

    ","

    anonInputs <- parseInputs

    ","

    anonPlatform <- textParser

    ","

    anonBuilder <- textParser

    ","

    anonArgs <- vectorOf textParser

    ","

    let keyValue1 = do
            "("
            key <- textParser
            ","
            value <- textParser
            ")"
            pure (key, value)
    anonEnv <- mapOf keyValue1

    ")"

    pure TraditionalDerivation {..}

-- | Parse a derivation output
parseFreeformDerivationOutput :: StoreDir -> Parser FreeformDerivationOutput
parseFreeformDerivationOutput sd = do
    rawPath <- textParser
    ","
    rawMethodHashAlgo <- textParser
    ","
    rawHash <- textParser
    parseRawDerivationOutput sd $ RawDerivationOutput {..}

-- | Parse a derivation inputs
parseTraditionalDerivationInputs :: StoreDir -> Parser TraditionalDerivationInputs
parseTraditionalDerivationInputs sd = do
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
