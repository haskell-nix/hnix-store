{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}

-- | Shared types

module System.Nix.Derivation.Traditional
    ( RawDerivationOutput(..)
    , parseRawDerivationOutput
    , renderRawDerivationOutput
    , TraditionalDerivationInputs(..)
    , inputsToTraditional
    ) where


import Control.Monad (when)
import Control.DeepSeq (NFData(..))
import Data.Constraint.Extras (Has(has))
import Data.Dependent.Sum (DSum(..))
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Map.Monoidal qualified
import Data.Set (Set)
import Data.Some
import Data.Text (Text)
import Data.Text qualified
import Data.These (These(..))
import GHC.Generics (Generic)

import System.Nix.ContentAddress (ContentAddressMethod(..))
import System.Nix.Derivation
import System.Nix.Hash
import System.Nix.OutputName (OutputName, outputStoreObjectName)
import System.Nix.StorePath
import System.Nix.StorePath.ContentAddressed

-- | Useful for the ATerm format, and remote protocols that need the same parsing
-- If it won't for the protocol, we would just inline this into the ATerm code proper.
data RawDerivationOutput = RawDerivationOutput
    { rawPath :: Text
    , rawMethodHashAlgo :: Text
    , rawHash :: Text
    } deriving (Eq, Generic, Ord, Show)

parseRawDerivationOutput
  :: forall m
  .  MonadFail m
  => StoreDir
  -> StorePathName
  -> OutputName
  -> RawDerivationOutput
  -> m DerivationOutput
parseRawDerivationOutput storeDir drvName outputName (RawDerivationOutput {..}) = do
    let onNonEmptyText :: Text -> (Text -> m a) -> m (Maybe a)
        onNonEmptyText = flip $ \f -> \case
          "" -> pure Nothing
          t -> Just <$> f t
    mPath <- onNonEmptyText rawPath $ \t -> case System.Nix.StorePath.parsePathFromText storeDir t of
      Left e -> fail $ show e -- TODO
      Right sp -> pure sp
    mHashAlgo <- onNonEmptyText rawMethodHashAlgo splitMethodHashAlgo
    mHash <- onNonEmptyText rawHash pure
    case (mPath, mHashAlgo, mHash) of
        (Just path, Nothing, Nothing) ->
              pure InputAddressedDerivationOutput {..}
        (Just path, Just (method, Some hashAlgo), Just hash0) -> do
              hash' <- either fail pure $ has @NamedAlgo hashAlgo $
                  decodeDigestWith NixBase32 hash0
              let hash = hashAlgo :=> hash'
              let expectedPath = makeFixedOutputPath storeDir method hash mempty $ outputStoreObjectName drvName outputName
              when (path /= expectedPath) $
                fail "fixed output path does not match info"
              pure FixedDerivationOutput {..}
        (Nothing, Just (method, hashAlgo), Nothing) -> do
              pure ContentAddressedDerivationOutput {..}
        _ ->
            fail "bad output in derivation"

renderRawDerivationOutput
    :: StoreDir
    -> StorePathName
    -> OutputName
    -> DerivationOutput
    -> RawDerivationOutput
renderRawDerivationOutput storeDir drvName outputName = \case
  InputAddressedDerivationOutput {..} -> RawDerivationOutput
    { rawPath = storePathToText storeDir path
    , rawMethodHashAlgo = ""
    , rawHash = ""
    }
  FixedDerivationOutput {..} -> case hash of
    hashAlgo :=> hash' -> RawDerivationOutput
      { rawPath = storePathToText storeDir $ makeFixedOutputPath storeDir method hash mempty $ outputStoreObjectName drvName outputName
      , rawMethodHashAlgo = buildMethodHashAlgo method $ Some hashAlgo
      , rawHash = encodeDigestWith NixBase32 hash'
      }
  ContentAddressedDerivationOutput {..} -> RawDerivationOutput
      { rawPath = ""
      , rawMethodHashAlgo = buildMethodHashAlgo method hashAlgo
      , rawHash = ""
      }

buildMethodHashAlgo :: ContentAddressMethod -> Some HashAlgo -> Text
buildMethodHashAlgo method hashAlgo = Data.Text.intercalate ":" $
  (case method of
    ContentAddressMethod_NixArchive -> ["r"]
    ContentAddressMethod_Text -> ["text"]
    ContentAddressMethod_Flat -> [])
  <>
  [withSome hashAlgo algoToText]

splitMethodHashAlgo :: MonadFail m => Text -> m (ContentAddressMethod, Some HashAlgo)
splitMethodHashAlgo methodHashAlgo = do
  (method, hashAlgoS) <- case Data.Text.splitOn ":" methodHashAlgo of
    ["r", hashAlgo] -> pure (ContentAddressMethod_NixArchive, hashAlgo)
    ["text", hashAlgo] -> pure (ContentAddressMethod_NixArchive, hashAlgo)
    [hashAlgo] -> pure (ContentAddressMethod_Flat, hashAlgo)
    _ -> fail "invalid number of colons or unknown CA method prefix"
  hashAlgo <- either fail pure $ textToAlgo hashAlgoS
  pure (method, hashAlgo)

-- | Useful for the ATerm format
data TraditionalDerivationInputs = TraditionalDerivationInputs
    { traditionalSrcs :: Set StorePath
    -- ^ Inputs that are sources
    , traditionalDrvs :: Map StorePath (Set OutputName)
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    } deriving (Eq, Generic, Ord, Show)

instance NFData TraditionalDerivationInputs

instance Semigroup TraditionalDerivationInputs where
  TraditionalDerivationInputs x0 x1 <> TraditionalDerivationInputs y0 y1 = TraditionalDerivationInputs
    (x0 <> y0)
    (x1 <> y1)

instance Monoid TraditionalDerivationInputs where
  mempty = TraditionalDerivationInputs mempty mempty

inputsToTraditional :: DerivationInputs -> Either StorePath TraditionalDerivationInputs
inputsToTraditional is = (\drvs -> TraditionalDerivationInputs
  { traditionalSrcs = srcs is
  , traditionalDrvs = drvs
  }) <$> go (drvs is)
  where
   go = fmap getMonoidalMap
     . Data.Map.Monoidal.traverseWithKey
       (\storePath -> (\case
           This os -> Right os
           _ -> Left storePath -- TODO make better error, e.g. by partitioning the map
        ) . unChildNode)
     . unDerivedPathMap
