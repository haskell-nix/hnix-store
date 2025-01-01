{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Fingerprint of Nix store path metadata used for signature verification
-}
module System.Nix.Fingerprint
  ( fingerprint
  , metadataFingerprint
  ) where

import Crypto.Hash (Digest)
import Data.Dependent.Sum (DSum)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Word (Word64)
import System.Nix.Hash (HashAlgo, algoDigestBuilder)
import System.Nix.StorePath
import System.Nix.StorePath.Metadata (Metadata(..))

import Data.HashSet qualified as HashSet
import Data.Text qualified as Text

-- | Produce the message signed by a NAR signature
metadataFingerprint :: StoreDir -> StorePath -> Metadata StorePath -> Text
metadataFingerprint storeDir storePath Metadata{..} = let
  narSize = fromMaybe 0 metadataNarBytes
  in fingerprint
       storeDir
       storePath
       metadataNarHash
       narSize
       (HashSet.toList metadataReferences)

-- | Produce the message signed by a NAR signature
fingerprint :: StoreDir
            -> StorePath
            -> DSum HashAlgo Digest -- ^ NAR hash
            -> Word64 -- ^ NAR size, in bytes
            -> [StorePath] -- ^ References
            -> Text
fingerprint storeDir storePath narHash narSize refs = let
  encodedStorePath = storePathToText storeDir storePath
  encodedNarHash = (toStrict . toLazyText . algoDigestBuilder) narHash
  encodedNarSize = (Text.pack . show) narSize
  sortedRefs = sort (storePathToText storeDir <$> refs)
  encodedRefs = Text.intercalate "," sortedRefs
  in Text.intercalate ";" [ "1", encodedStorePath, encodedNarHash, encodedNarSize, encodedRefs]

