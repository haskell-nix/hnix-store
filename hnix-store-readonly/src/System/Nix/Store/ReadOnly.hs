{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Store.ReadOnly
  ( References(..)
  , makeStorePath
  , makeFixedOutputPath
  , computeStorePathForPath
  ) where

import Control.Monad.State (StateT, execStateT, modify)
import Crypto.Hash (Context, Digest, SHA256)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import System.Nix.ContentAddress (ContentAddressMethod (..))
import System.Nix.Hash (BaseEncoding(Base16), NamedAlgo(algoName))
import System.Nix.Store.Types (PathFilter, RepairMode)
import System.Nix.StorePath (StoreDir, StorePath, StorePathName)

import qualified Crypto.Hash
import qualified Data.ByteString.Char8
import qualified Data.ByteString
import qualified Data.HashSet
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified System.Nix.Hash
import qualified System.Nix.Nar
import qualified System.Nix.StorePath

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

makeStorePath
  :: forall hashAlgo
   . (NamedAlgo hashAlgo)
  => StoreDir
  -> ByteString
  -> Digest hashAlgo
  -> StorePathName
  -> StorePath
makeStorePath storeDir ty h nm =
 System.Nix.StorePath.unsafeMakeStorePath storeHash nm
 where
  storeHash = System.Nix.StorePath.mkStorePathHashPart @hashAlgo s
  s =
    Data.ByteString.intercalate ":" $
      ty:fmap Data.Text.Encoding.encodeUtf8
        [ algoName @hashAlgo
        , System.Nix.Hash.encodeDigestWith Base16 h
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

makeTextPath
  :: NamedAlgo _SHA256
  => StoreDir
  -> Digest _SHA256 -- TODO enforce its it again
  -> HashSet StorePath
  -> StorePathName
  -> StorePath
makeTextPath storeDir h refs nm = makeStorePath storeDir ty h nm
 where
  ty = makeType storeDir "text" $ References
    { references_others = refs
    , references_self = False
    }

makeFixedOutputPath
  :: forall hashAlgo
  .  NamedAlgo hashAlgo
  => StoreDir
  -> ContentAddressMethod
  -> Digest hashAlgo
  -> References
  -> StorePathName
  -> StorePath
makeFixedOutputPath storeDir method h refs =
  case method of
    ContentAddressMethod_Text ->
      makeTextPath storeDir h $ references_others refs
    _ ->
      if method == ContentAddressMethod_NixArchive
         && (algoName @hashAlgo) == "sha256"
      then makeStorePath storeDir (makeType storeDir "source" refs) h
      else makeStorePath storeDir "output:out" h'
 where
  h' =
    Crypto.Hash.hash @ByteString @SHA256
      $  "fixed:out:"
      <> Data.Text.Encoding.encodeUtf8 (algoName @hashAlgo)
      <> (if method == ContentAddressMethod_NixArchive then ":r:" else ":")
      <> Data.Text.Encoding.encodeUtf8 (System.Nix.Hash.encodeDigestWith Base16 h)
      <> ":"

digestPath
  :: FilePath             -- ^ Local `FilePath` to add
  -> ContentAddressMethod -- ^ target directory method
  -> PathFilter           -- ^ Path filter function
  -> RepairMode           -- ^ Only used by local store backend
  -> IO (Digest SHA256)
digestPath pth method _pathFilter _repair =
  case method of
    ContentAddressMethod_Flat -> flatContentHash
    ContentAddressMethod_NixArchive -> nixArchiveContentHash
    ContentAddressMethod_Text -> flatContentHash
 where
  nixArchiveContentHash :: IO (Digest SHA256)
  nixArchiveContentHash =
    Crypto.Hash.hashFinalize
    <$> execStateT streamNarUpdate (Crypto.Hash.hashInit @SHA256)

  streamNarUpdate :: StateT (Context SHA256) IO ()
  streamNarUpdate =
    System.Nix.Nar.streamNarIO
      System.Nix.Nar.narEffectsIO
      pth
      (modify . flip (Crypto.Hash.hashUpdate @ByteString @SHA256))

  flatContentHash :: IO (Digest SHA256)
  flatContentHash =
    Crypto.Hash.hashlazy
    <$> System.Nix.Nar.narReadFile
          System.Nix.Nar.narEffectsIO
          pth

computeStorePathForPath
  :: StoreDir
  -> StorePathName        -- ^ Name part of the newly created `StorePath`
  -> FilePath             -- ^ Local `FilePath` to add
  -> ContentAddressMethod -- ^ Add target directory methodly
  -> PathFilter           -- ^ Path filter function
  -> RepairMode           -- ^ Only used by local store backend
  -> IO StorePath
computeStorePathForPath storeDir name pth method pathFilter repair = do
  selectedHash <- digestPath pth method pathFilter repair
  pure $ makeFixedOutputPath storeDir method selectedHash mempty name
