{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ReadonlyStore
  ( makeStorePath
  , makeTextPath
  , makeFixedOutputPath
  , computeStorePathForText
  , computeStorePathForPath
  ) where

import Control.Monad.State (StateT, execStateT, modify)
import Crypto.Hash (Context, Digest, SHA256)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import System.Nix.Hash (BaseEncoding(Base16), NamedAlgo(algoName))
import System.Nix.Store.Types (FileIngestionMethod(..))
import System.Nix.StorePath (StoreDir, StorePath(StorePath), StorePathName)

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
  :: forall hashAlgo
   . (NamedAlgo hashAlgo)
  => StoreDir
  -> ByteString
  -> Digest hashAlgo
  -> StorePathName
  -> StorePath
makeStorePath storeDir ty h nm = StorePath storeHash nm
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

makeTextPath
  :: StoreDir
  -> StorePathName
  -> Digest SHA256
  -> HashSet StorePath
  -> StorePath
makeTextPath storeDir nm h refs = makeStorePath storeDir ty h nm
 where
  ty =
    Data.ByteString.intercalate
      ":"
      $ "text"
      : Data.List.sort
          (System.Nix.StorePath.storePathToRawFilePath storeDir
           <$> Data.HashSet.toList refs)

makeFixedOutputPath
  :: forall hashAlgo
  .  NamedAlgo hashAlgo
  => StoreDir
  -> FileIngestionMethod
  -> Digest hashAlgo
  -> StorePathName
  -> StorePath
makeFixedOutputPath storeDir recursive h =
  if recursive == FileIngestionMethod_FileRecursive
     && (algoName @hashAlgo) == "sha256"
  then makeStorePath storeDir "source" h
  else makeStorePath storeDir "output:out" h'
 where
  h' =
    Crypto.Hash.hash @ByteString @SHA256
      $  "fixed:out:"
      <> Data.Text.Encoding.encodeUtf8 (algoName @hashAlgo)
      <> (if recursive == FileIngestionMethod_FileRecursive then ":r:" else ":")
      <> Data.Text.Encoding.encodeUtf8 (System.Nix.Hash.encodeDigestWith Base16 h)
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
  -> (FilePath -> Bool)   -- ^ Path filter function
  -> Bool                 -- ^ Only used by local store backend
  -> IO StorePath
computeStorePathForPath storeDir name pth recursive _pathFilter _repair = do
  selectedHash <-
    if recursive == FileIngestionMethod_FileRecursive
      then recursiveContentHash
      else flatContentHash
  pure $ makeFixedOutputPath storeDir recursive selectedHash name
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
