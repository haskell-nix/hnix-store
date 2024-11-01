{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module System.Nix.Derivation
  ( parseDerivation
  , buildDerivation
  , DerivationInputs(..)
  -- Re-exports
  , Derivation(..)
  , DerivationOutput(..)
  ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.Set (Set)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import GHC.Generics (Generic)

import Nix.Derivation (Derivation(..))
import System.Nix.ContentAddress (ContentAddress(..), ContentAddressMethod(..))
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..))
import System.Nix.Hash (HashAlgo)
import System.Nix.StorePath (StoreDir, StorePath)
import System.Nix.OutputName (OutputName)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

import qualified Nix.Derivation
import qualified System.Nix.OutputName
import qualified System.Nix.Hash
import qualified System.Nix.StorePath

newtype DerivationInputs _fp _outputName = DerivationInputs
  { unDerivationInputs :: Set DerivedPath }
  deriving (Show, Eq, Ord, Generic)

data DerivationOutput _fp
  = DerivationOutput_InputAddressed StorePath
  | DerivationOutput_CAFixed StorePath ContentAddress
  | DerivationOutput_CAFloating (Some HashAlgo)
  | DerivationOutput_Deferred
  deriving (Show, Eq, Ord, Generic)

parseDerivation
  :: StoreDir
  -> Parser (Derivation
              StorePath
              Text
              OutputName
              DerivationOutput
              DerivationInputs
            )
parseDerivation expectedRoot =
  Nix.Derivation.parseDerivationWith
    Nix.Derivation.textParser
    outputName
    fixedOutputParser
    inputsParser
  where
    outputName = do
      text <- Nix.Derivation.textParser
        -- System.Nix.OutputName.outputNameParser ?
      case System.Nix.OutputName.mkOutputName text of
        Left e -> fail (show e)
        Right o -> pure o
    pathParser = do
      text <- Nix.Derivation.textParser
      case Data.Attoparsec.Text.Lazy.parseOnly
            (System.Nix.StorePath.pathParser expectedRoot)
            (Data.Text.Lazy.fromStrict text)
        of
          Right p -> pure p
          Left e -> fail e

    fixedOutputParser :: Parser (DerivationOutput StorePath)
    fixedOutputParser = do
      --path <- pathParser
      tpath <- Nix.Derivation.textParser
      ","
      hashName <- Nix.Derivation.textParser
      ","
      digest <- Nix.Derivation.textParser
      -- oof
      -- * rename derivationOutput to RealisationDerivationOutput
      -- or just RealisationOutput or something fitting
      -- * drop its param and fix OutputName to it
      -- * use separate DerivationOutput type for Derivations
      -- if hashName == mempty || digest == mempty
      if
          | tpath /= mempty && hashName == mempty && digest == mempty ->
              case Data.Attoparsec.Text.Lazy.parseOnly
                    (System.Nix.StorePath.pathParser expectedRoot)
                    (Data.Text.Lazy.fromStrict tpath)
                of
                  Left e -> fail e
                  Right path -> pure $ DerivationOutput_InputAddressed path

          | tpath /= mempty && hashName /= mempty && digest /= mempty ->
              case System.Nix.Hash.mkNamedDigest hashName digest of
                Left e -> error (show e) -- fail (show e)
                Right namedDigest ->
                  case Data.Attoparsec.Text.Lazy.parseOnly
                        (System.Nix.StorePath.pathParser expectedRoot)
                        (Data.Text.Lazy.fromStrict tpath)
                    of
                      Left e -> fail e
                      Right path ->
                        pure
                        $ DerivationOutput_CAFixed
                            path
                            (ContentAddress TextIngestionMethod namedDigest)
                            -- TODO: ^ parse CAMethod from prefix
                            -- ContentAddressMethod is determited
                            -- by parsing a prefix 'r:' for FileIngestionMethod::Recursive
          | tpath == mempty && hashName /= mempty && digest == mempty ->
              pure undefined -- CAFloating (Some HashAlgo)
          | otherwise ->
              fail "bad output in derivation"

    inputsParser :: Parser (DerivationInputs StorePath OutputName)
    inputsParser = do
      drvs <- listOf $ do
        "("
        path <- pathParser
        ","
        outputNames <- listOf outputName
        ")"
        pure
          $ DerivedPath_Built
              path
              (OutputsSpec_Names
                (Data.Set.fromList outputNames)
              )

      ","
      srcs <- fmap DerivedPath_Opaque <$> listOf pathParser
      pure
        $ DerivationInputs
        $ Data.Set.fromList (drvs ++ srcs)

    -- stolen from nix-derivation (BSD3)
    listOf :: Parser a -> Parser [a]
    listOf element = do
      "["
      es <- Data.Attoparsec.Text.Lazy.sepBy element ","
      "]"
      pure es


buildDerivation
  :: StoreDir
  -> Derivation
       StorePath
       Text
       OutputName
       DerivationOutput
       DerivationInputs
  -> Builder
buildDerivation storeDir =
  Nix.Derivation.buildDerivationWith
    string
    (string . System.Nix.OutputName.unOutputName)
    fixedOutputBuilder
    inputsBuilder
  where
    string = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show

    storePath = string . System.Nix.StorePath.storePathToText storeDir

    fixedOutputBuilder :: DerivationOutput StorePath -> Builder
    fixedOutputBuilder (DerivationOutput_InputAddressed path) =
         storePath path
      <> ","
      <> string mempty
      <> ","
      <> string mempty
    fixedOutputBuilder (DerivationOutput_CAFixed path (ContentAddress _method digest)) =
         storePath path
      <> ","
      <> System.Nix.Hash.algoDigestBuilderSep
          ','
          digest

    inputsBuilder :: DerivationInputs fp outputName -> Builder
    inputsBuilder DerivationInputs{..} =
      let
        isBuilt (DerivedPath_Built _ _) = True
        isBuilt _ = False

        (drvs, srcs) = Data.Set.partition isBuilt unDerivationInputs

        buildOutputsSpec (OutputsSpec_All) = error "..."
        buildOutputsSpec (OutputsSpec_Names nameSet) =
           listOf (string . System.Nix.OutputName.unOutputName) (Data.Set.toList nameSet)

        buildDrv (DerivedPath_Built path outputsSpec) =
             "("
          <> storePath path
          <> ","
          <> buildOutputsSpec outputsSpec
          <> ")"
        buildDrv _ = mempty

        buildSrc (DerivedPath_Opaque path) = storePath path
        buildSrc _ = mempty
      in
           setOf buildDrv drvs
        <> ","
        <> setOf buildSrc srcs

    -- stolen from nix-derivation (BSD3)
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
