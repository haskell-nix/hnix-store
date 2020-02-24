{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Description : Metadata about Nix store paths.
-}
module System.Nix.StorePathMetadata where

import System.Nix.StorePath (ContentAddressableAddress (..), NarHashMode (..), StorePath, StorePathSet, storePathName, unStorePathName)
import System.Nix.Hash (Digest, NamedAlgo, SomeNamedDigest, algoName, encodeBase16, encodeSomeDigest)
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Text as T
import Data.Word (Word64)
import System.Nix.Signature (NarSignature)

-- | Metadata about a 'StorePath' in @storeDir@.
data StorePathMetadata storeDir = StorePathMetadata
  { -- | The path this metadata is about
    path :: !(StorePath storeDir)
  , -- | The path to the derivation file that built this path, if any
    -- and known.
    deriverPath :: !(Maybe (StorePath storeDir))
  , -- TODO should this be optional?
    -- | The hash of the nar serialization of the path.
    narHash :: !SomeNamedDigest
  , -- | The paths that this path directly references
    references :: !(StorePathSet storeDir)
  , -- | When was this path registered valid in the store?
    registrationTime :: !UTCTime
  , -- | The size of the nar serialization of the path, in bytes.
    narBytes :: !(Maybe Word64)
  , -- | How much we trust this path.
    trust :: !StorePathTrust
  , -- | A set of cryptographic attestations of this path's validity.
    --
    -- There is no guarantee from this type alone that these
    -- signatures are valid.
    sigs :: !(Set NarSignature)
  , -- | Whether and how this store path is content-addressable.
    --
    -- There is no guarantee from this type alone that this address
    -- is actually correct for this store path.
    contentAddressableAddress :: !(Maybe ContentAddressableAddress)
  }

-- | How much do we trust the path, based on its provenance?
data StorePathTrust
  = -- | It was built locally and thus ultimately trusted
    BuiltLocally
  | -- | It was built elsewhere (and substituted or similar) and so
    -- is less trusted
    BuiltElsewhere

data NarInfo
  = NarInfo
  { -- | URL to compressed Nar file.
    narUrl :: Text
  , -- | Form of compression used on Nar file.
    narCompression :: Text
  , -- | The hash of the nar serialization of the path.
    fileHash :: !SomeNamedDigest
  , -- | The size of the nar the path, in bytes.
    fileBytes :: !(Maybe Word64)
  }

-- | Serialize to .narinfo contents
encodeNarInfo
  :: StorePathMetadata storeDir
  -> NarInfo
  -> Text
encodeNarInfo storePathMetadata narInfo =
  T.unlines
    [ "StorePath: " <> unStorePathName (storePathName (path storePathMetadata))
    , "URL: " <> narUrl narInfo
    , "Compression: " <> narCompression narInfo
    , "FileHash: " <> encodeSomeDigest (fileHash narInfo)
    , "FileSize: " <> foldMap (T.pack . show) (fileBytes narInfo)
    , "NarHash: " <> encodeSomeDigest (narHash storePathMetadata)
    , "NarSize: " <> foldMap (T.pack . show) (narBytes storePathMetadata)
    , "References: "
    , "CA: " <> foldMap encodeContentAddressableAddress (contentAddressableAddress storePathMetadata)
    ]
  where
    encodeContentAddressableAddress (Text d) =
      "text:sha256:" <> encodeBase16 d
    encodeContentAddressableAddress (Fixed m d) =
      "fixed:" <> encodeNarHashMode m <> ":" <> encodeSomeDigest d
    encodeNarHashMode RegularFile =
      ""
    encodeNarHashMode Recursive =
      "r:"
