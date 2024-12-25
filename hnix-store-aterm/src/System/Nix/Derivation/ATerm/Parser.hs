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

import Control.Monad (when)
import Data.Attoparsec.Text.Lazy (Parser)
import Data.Constraint.Extras (Has(has))
import Data.Dependent.Sum
import Data.These (These(This))
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Set (Set)
import Data.Some
import Data.Text (Text)
import Data.Vector (Vector)
import System.Nix.ContentAddress
import System.Nix.Derivation
    ( Derivation
    , Derivation'(..)
    , DerivationOutput(..)
    , DerivedPathMap(..)
    , ChildNode(..)
    , DerivationInputs(..)
    )
import System.Nix.Hash
import System.Nix.StorePath
import System.Nix.StorePath.ContentAddressed
import System.Nix.OutputName

import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Vector

listOf :: Parser a -> Parser [a]
listOf element = do
    "["
    es <- Data.Attoparsec.Text.Lazy.sepBy element ","
    "]"
    pure es

-- | Parse a derivation
parseDerivation :: StoreDir -> StorePathName -> Parser Derivation
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

splitMethodHashAlgo :: Text -> Parser (ContentAddressMethod, Some HashAlgo)
splitMethodHashAlgo methodHashAlgo = do
  (method, hashAlgoS) <- case Data.Text.splitOn ":" methodHashAlgo of
    ["r", hashAlgo] -> pure (ContentAddressMethod_NixArchive, hashAlgo)
    ["text", hashAlgo] -> pure (ContentAddressMethod_NixArchive, hashAlgo)
    [hashAlgo] -> pure (ContentAddressMethod_Flat, hashAlgo)
    _ -> fail "invalid number of colons or unknown CA method prefix"
  hashAlgo <- either fail pure $ textToAlgo hashAlgoS
  pure (method, hashAlgo)

-- | Parse a derivation output
parseDerivationOutput :: StoreDir -> StorePathName -> OutputName -> Parser DerivationOutput
parseDerivationOutput sd drvName outputName = do
    mPath <- maybeTextParser $ storePathParser sd
    ","
    mHashAlgo <- maybeTextParser textParser
    ","
    mHash <- maybeTextParser textParser
    case (mPath, mHashAlgo, mHash) of
        (Just path, Nothing, Nothing) ->
              pure InputAddressedDerivationOutput {..}
        (Just path, Just methodHashAlgo, Just hash0) -> do
              (method, Some hashAlgo) <- splitMethodHashAlgo methodHashAlgo
              hash' <- either fail pure $ has @NamedAlgo hashAlgo $
                  decodeDigestWith NixBase32 hash0
              let hash = hashAlgo :=> hash'
              let expectedPath = makeFixedOutputPath sd method hash mempty $ outputStoreObjectName drvName outputName
              when (path /= expectedPath) $
                fail "fixed output path does not match info"
              pure FixedDerivationOutput {..}
        (Nothing, Just methodHashAlgo, Nothing) -> do
              (method, hashAlgo) <- splitMethodHashAlgo methodHashAlgo
              pure ContentAddressedDerivationOutput {..}
        _ ->
            fail "bad output in derivation"

-- | Parse a derivation inputs
parseDerivationInputs :: StoreDir -> Parser DerivationInputs
parseDerivationInputs sd = do
    let keyValue = do
            "("
            key <- storePathParser sd
            ","
            value <- setOf outputNameParser
            ")"
            pure
                ( key
                , ChildNode $ This value
                )
    drvs <- DerivedPathMap . MonoidalMap <$> mapOf keyValue

    ","

    srcs <- setOf $ storePathParser sd
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
