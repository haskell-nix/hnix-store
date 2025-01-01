{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nix.StorePath.ContentAddressed
  ( References(..)
  , makeStorePath
  , makeFixedOutputPath
  ) where

import Crypto.Hash (Digest, SHA256, HashAlgorithm)
import Data.ByteString (ByteString)
import Data.Constraint.Extras (Has(has))
import Data.Dependent.Sum (DSum((:=>)))
import Data.HashSet (HashSet)
import Data.Some (Some(Some))
import System.Nix.ContentAddress (ContentAddressMethod (..))
import System.Nix.Hash (BaseEncoding(Base16), HashAlgo(..))
import System.Nix.StorePath (StoreDir, StorePath, StorePathName)

import Crypto.Hash qualified
import Data.ByteString.Char8 qualified
import Data.ByteString qualified
import Data.HashSet qualified
import Data.List qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import System.Nix.Hash qualified
import System.Nix.StorePath qualified

data References = References
  { references_others :: HashSet StorePath
  , references_self :: Bool
  }

instance Semigroup References where
  a <> b = References
    { references_others = references_others a <> references_others b
    , references_self = references_self a || references_self b
    }

instance Monoid References where
  mempty = References
    { references_others = mempty
    , references_self = False
    }

-- | TODO this isn't just for content-addrssed paths, move elsewhere
makeStorePath
  :: StoreDir
  -> ByteString
  -> DSum HashAlgo Digest
  -> StorePathName
  -> StorePath
makeStorePath storeDir ty (hashAlgo :=> (digest :: Digest a)) nm =
 System.Nix.StorePath.unsafeMakeStorePath storeHash nm
 where
  storeHash = has @HashAlgorithm hashAlgo $ System.Nix.StorePath.mkStorePathHashPart @a s
  s =
    Data.ByteString.intercalate ":" $
      ty:fmap Data.Text.Encoding.encodeUtf8
        [ System.Nix.Hash.algoToText hashAlgo
        , System.Nix.Hash.encodeDigestWith Base16 digest
        , Data.Text.pack . Data.ByteString.Char8.unpack $ System.Nix.StorePath.unStoreDir storeDir
        , System.Nix.StorePath.unStorePathName nm
        ]

makeType
  :: StoreDir
  -> ByteString
  -> References
  -> ByteString
makeType storeDir ty refs =
  Data.ByteString.intercalate ":" $ ty : (others ++ self)
  where
    others = Data.List.sort
      $ fmap (System.Nix.StorePath.storePathToRawFilePath storeDir)
      $ Data.HashSet.toList
      $ references_others refs
    self = ["self" | references_self refs]

makeFixedOutputPath
  :: StoreDir
  -> ContentAddressMethod
  -> DSum HashAlgo Digest
  -> References
  -> StorePathName
  -> StorePath
makeFixedOutputPath storeDir method digest@(hashAlgo :=> h) refs =
  makeStorePath storeDir ty digest'
 where
  (ty, digest') = case method of
    ContentAddressMethod_Text ->
      case hashAlgo of
        HashAlgo_SHA256
          | references_self refs == False -> (makeType storeDir "text" refs, digest)
        _ -> error "unsupported" -- TODO do better; maybe we'll just remove this restriction too?
    _ ->
      if method == ContentAddressMethod_NixArchive
         && Some hashAlgo == Some HashAlgo_SHA256
      then (makeType storeDir "source" refs, digest)
      else let
        h' =
          Crypto.Hash.hash @ByteString @SHA256
            $  "fixed:out:"
            <> Data.Text.Encoding.encodeUtf8 (System.Nix.Hash.algoToText hashAlgo)
            <> (if method == ContentAddressMethod_NixArchive then ":r:" else ":")
            <> Data.Text.Encoding.encodeUtf8 (System.Nix.Hash.encodeDigestWith Base16 h)
            <> ":"
      in ("output:out", HashAlgo_SHA256 :=> h')
