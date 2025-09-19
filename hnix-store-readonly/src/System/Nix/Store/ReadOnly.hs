{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Store.ReadOnly
  ( computeStorePathForPath
  ) where

import Control.Monad.State (StateT, execStateT, modify)
import Crypto.Hash (Context, Digest, SHA256)
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum((:=>)))
import System.Nix.StorePath.ContentAddressed
import System.Nix.ContentAddress (ContentAddressMethod (..))
import System.Nix.Hash (HashAlgo(..))
import System.Nix.Store.Types (PathFilter, RepairMode)
import System.Nix.StorePath (StoreDir, StorePath, StorePathName)

import Crypto.Hash qualified
import System.Nix.Nar qualified

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
  pure $ makeFixedOutputPath storeDir method (HashAlgo_SHA256 :=> selectedHash) mempty name
