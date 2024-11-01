{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Store.ReadOnly
  ( makeStorePath
  , makeTextPath
  , makeFixedOutputPath
  , computeStorePathForText
  , computeStorePathForPath
  ) where

import Control.Monad.State (StateT, execStateT, modify)
import Crypto.Hash (Context, Digest, SHA256, HashAlgorithm)
import Data.ByteString (ByteString)
import Data.Constraint.Extras (Has(has))
import Data.Dependent.Sum (DSum((:=>)))
import Data.HashSet (HashSet)
import Data.Some (Some(Some))
import System.Nix.Hash (BaseEncoding(Base16), HashAlgo(..))
import System.Nix.Store.Types (FileIngestionMethod(..), PathFilter, RepairMode)
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

makeTextPath
  :: StoreDir
  -> StorePathName
  -> Digest SHA256
  -> HashSet StorePath
  -> StorePath
makeTextPath storeDir nm h refs = makeStorePath storeDir ty (HashAlgo_SHA256 :=> h) nm
 where
  ty =
    Data.ByteString.intercalate
      ":"
      $ "text"
      : Data.List.sort
          (System.Nix.StorePath.storePathToRawFilePath storeDir
           <$> Data.HashSet.toList refs)

makeFixedOutputPath
  :: StoreDir
  -> FileIngestionMethod
  -> DSum HashAlgo Digest
  -> StorePathName
  -> StorePath
makeFixedOutputPath storeDir recursive algoDigest@(hashAlgo :=> digest) =
  if recursive == FileIngestionMethod_FileRecursive
     && Some hashAlgo == Some HashAlgo_SHA256
  then makeStorePath storeDir "source" algoDigest
  else makeStorePath storeDir "output:out" (HashAlgo_SHA256 :=> h')
 where
  h' =
    Crypto.Hash.hash @ByteString @SHA256
      $  "fixed:out:"
      <> Data.Text.Encoding.encodeUtf8 (System.Nix.Hash.algoToText hashAlgo)
      <> (if recursive == FileIngestionMethod_FileRecursive then ":r:" else ":")
      <> Data.Text.Encoding.encodeUtf8 (System.Nix.Hash.encodeDigestWith Base16 digest)
      <> ":"

computeStorePathForText
  :: StoreDir
  -> StorePathName
  -> ByteString
  -> (HashSet StorePath -> StorePath)
computeStorePathForText storeDir nm =
  makeTextPath storeDir nm
  . Crypto.Hash.hash

computeStorePathForPath
  :: StoreDir
  -> StorePathName        -- ^ Name part of the newly created `StorePath`
  -> FilePath             -- ^ Local `FilePath` to add
  -> FileIngestionMethod  -- ^ Add target directory recursively
  -> PathFilter           -- ^ Path filter function
  -> RepairMode           -- ^ Only used by local store backend
  -> IO StorePath
computeStorePathForPath storeDir name pth recursive _pathFilter _repair = do
  selectedHash <-
    if recursive == FileIngestionMethod_FileRecursive
      then recursiveContentHash
      else flatContentHash
  pure $ makeFixedOutputPath storeDir recursive (HashAlgo_SHA256 :=> selectedHash) name
 where
  recursiveContentHash :: IO (Digest SHA256)
  recursiveContentHash =
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
