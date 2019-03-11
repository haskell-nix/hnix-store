{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.ReadonlyStore where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import           Data.Text (Text)
import           Data.Text.Encoding
import           System.Nix.Hash
import           System.Nix.StorePath

makeStorePath :: forall storeDir hashAlgo . (KnownStoreDir storeDir, NamedAlgo hashAlgo) => ByteString -> Digest hashAlgo -> StorePathName -> StorePath storeDir
makeStorePath ty h nm = StorePath storeHash nm
  where
    s = BS.intercalate ":"
      [ ty
      , encodeUtf8 $ algoName @hashAlgo
      , encodeUtf8 $ encodeBase16 h
      , storeDirVal @storeDir
      , encodeUtf8 $ unStorePathName nm
      ]
    storeHash = hash s

makeTextPath :: (KnownStoreDir storeDir) => StorePathName -> Digest 'SHA256 -> StorePathSet storeDir -> StorePath storeDir
makeTextPath nm h refs = makeStorePath ty h nm
  where
    ty = BS.intercalate ":" ("text" : map storePathToRawFilePath (HS.toList refs))

makeFixedOutputPath :: (KnownStoreDir storeDir, ValidAlgo hashAlgo, NamedAlgo hashAlgo) => Bool -> Digest hashAlgo -> StorePathName -> StorePath storeDir
makeFixedOutputPath recursive h nm =
  makeStorePath ty h' nm
  where
    (ty, h') =
      if recursive
      then ("source", h)
      else ("output:out", hash ("fixed:out:" <> encodeUtf8 (encodeBase16 h) <> ":"))

makeFixedOutputCA :: Bool -> Digest hashAlgo -> Text
makeFixedOutputCA recursive h =
  "fixed:" <> (if recursive then "r:" else "") <> encodeBase16 h

computeStorePathForText :: (KnownStoreDir storeDir) => StorePathName -> ByteString -> StorePathSet storeDir -> StorePath storeDir
computeStorePathForText nm s refs = makeTextPath nm (hash s) refs
