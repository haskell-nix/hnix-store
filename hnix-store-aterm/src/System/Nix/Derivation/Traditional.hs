{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}

-- | Shared types

module System.Nix.Derivation.Traditional
    ( RawDerivationOutput(..)
    , parseRawDerivationOutput
    , renderRawDerivationOutput
    , TraditionalDerivation'(..)
    , withName
    , withoutName
    , TraditionalDerivationInputs(..)
    , inputsToTraditional
    , inputsFromTraditional
    ) where


import Control.DeepSeq (NFData(..))
import Data.Constraint.Extras (Has(has))
import Data.Dependent.Sum (DSum(..))
import Data.Map (Map)
import Data.Map qualified
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Map.Monoidal qualified
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Some
import Data.Text (Text)
import Data.Text qualified
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Traversable (for)
import GHC.Generics (Generic, (:.:)(..))

import System.Nix.ContentAddress (ContentAddressMethod(..))
import System.Nix.Derivation
import System.Nix.Hash
import System.Nix.OutputName (OutputName)
import System.Nix.StorePath

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
  -> RawDerivationOutput
  -> m FreeformDerivationOutput
parseRawDerivationOutput storeDir (RawDerivationOutput {..}) = do
  let onNonEmptyText :: Text -> (Text -> m a) -> m (Maybe a)
      onNonEmptyText = flip $ \f -> \case
        "" -> pure Nothing
        t -> Just <$> f t
  mPath <- onNonEmptyText rawPath $ \t -> case System.Nix.StorePath.parsePathFromText storeDir t of
    Left e -> fail $ show e -- TODO
    Right sp -> pure sp
  mMethodHashAlgo <- onNonEmptyText rawMethodHashAlgo splitMethodHashAlgo
  mHash0 <- onNonEmptyText rawHash pure
  mContentAddressing <- case mMethodHashAlgo of
    Nothing -> case mHash0 of
      Nothing -> pure Nothing
      Just _ -> fail "Hash without method and hash algo is not allowed"
    Just (method, Some hashAlgo) -> do
      mHash <- for mHash0 $ \hash0 ->
        either fail pure $ has @NamedAlgo hashAlgo $
          decodeDigestWith NixBase32 hash0
      pure $ Just (method, hashAlgo :=> Comp1 mHash)
  pure FreeformDerivationOutput { mPath, mContentAddressing }

renderRawDerivationOutput
    :: StoreDir
    -> FreeformDerivationOutput
    -> RawDerivationOutput
renderRawDerivationOutput storeDir (FreeformDerivationOutput {..}) =
  RawDerivationOutput
    { rawPath = fromMaybe "" $ storePathToText storeDir <$> mPath
    , rawMethodHashAlgo = flip (maybe "") mContentAddressing $ \(method, hashAlgo :=> _) ->
        buildMethodHashAlgo method $ Some hashAlgo
    , rawHash = fromMaybe "" $ mContentAddressing >>= \(_, _ :=> Comp1 hash') ->
        encodeDigestWith NixBase32 <$> hash'
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
    ["text", hashAlgo] -> pure (ContentAddressMethod_Text, hashAlgo)
    [hashAlgo] -> pure (ContentAddressMethod_Flat, hashAlgo)
    _ -> fail "invalid number of colons or unknown CA method prefix"
  hashAlgo <- either fail pure $ textToAlgo hashAlgoS
  pure (method, hashAlgo)

----------------

-- | The ATerm format doesn't include the derivation name. That must
-- instead be gotten out of band, e.g. from the Store Path.
data TraditionalDerivation' inputs outputs = TraditionalDerivation
    { anonOutputs   :: outputs
    -- ^ Outputs produced by this derivation where keys are output names
    , anonInputs    :: inputs
    -- ^ Inputs (sources and derivations)
    , anonPlatform  :: Text
    -- ^ Platform required for this derivation
    , anonBuilder   :: Text
    -- ^ Code to build the derivation, which can be a path or a builtin function
    , anonArgs      :: Vector Text
    -- ^ Arguments passed to the executable used to build to derivation
    , anonEnv       :: Map Text Text
    -- ^ Environment variables provided to the executable used to build the
    -- derivation
    } deriving (Eq, Generic, Ord, Show)

instance (NFData inputs, NFData outputs) => NFData (TraditionalDerivation' inputs outputs)

withName :: StorePathName -> TraditionalDerivation' inputs outputs -> Derivation' inputs outputs
withName name drv0 = Derivation
  { name = name
  , outputs = anonOutputs drv0
  , inputs = anonInputs drv0
  , platform = anonPlatform drv0
  , builder = anonBuilder drv0
  , args = anonArgs drv0
  , env = anonEnv drv0
  }

withoutName :: Derivation' inputs outputs -> TraditionalDerivation' inputs outputs
withoutName drv0 = TraditionalDerivation
  { anonOutputs = outputs drv0
  , anonPlatform = platform drv0
  , anonInputs = inputs drv0
  , anonBuilder = builder drv0
  , anonArgs = args drv0
  , anonEnv = env drv0
  }

----------------

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

inputsFromTraditional :: TraditionalDerivationInputs -> DerivationInputs
inputsFromTraditional TraditionalDerivationInputs { traditionalSrcs, traditionalDrvs } = DerivationInputs
  { srcs = traditionalSrcs
  , drvs = DerivedPathMap $ Data.Map.Monoidal.fromList $
      fmap (fmap ChildNode . fmap This) (Data.Map.toList traditionalDrvs)
  }
