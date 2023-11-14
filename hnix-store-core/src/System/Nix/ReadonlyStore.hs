{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}

module System.Nix.ReadonlyStore where


import qualified Data.ByteString.Char8         as Bytes.Char8
import qualified Data.ByteString               as BS
import qualified Data.HashSet                  as HS
import           System.Nix.Hash
import           System.Nix.Nar
import           System.Nix.StorePath
import           Crypto.Hash                    ( Context
                                                , Digest
                                                , hash
                                                , hashlazy
                                                , hashInit
                                                , hashUpdate
                                                , hashFinalize
                                                , SHA256
                                                )


makeStorePath
  :: forall h
   . (NamedAlgo h)
  => StoreDir
  -> ByteString
  -> Digest h
  -> StorePathName
  -> StorePath
makeStorePath storeDir ty h nm = StorePath (coerce storeHash) nm
 where
  storeHash = mkStorePathHash @h s
  s =
    BS.intercalate ":" $
      ty:fmap encodeUtf8
        [ algoName @h
        , encodeDigestWith Base16 h
        , toText . Bytes.Char8.unpack $ unStoreDir storeDir
        , unStorePathName nm
        ]

makeTextPath
  :: StoreDir -> StorePathName -> Digest SHA256 -> StorePathSet -> StorePath
makeTextPath storeDir nm h refs = makeStorePath storeDir ty h nm
 where
  ty =
    BS.intercalate ":" $ "text" : sort (storePathToRawFilePath storeDir <$> HS.toList refs)

makeFixedOutputPath
  :: forall hashAlgo
  .  NamedAlgo hashAlgo
  => StoreDir
  -> Bool
  -> Digest hashAlgo
  -> StorePathName
  -> StorePath
makeFixedOutputPath storeDir recursive h =
  if recursive && (algoName @hashAlgo) == "sha256"
    then makeStorePath storeDir "source" h
    else makeStorePath storeDir "output:out" h'
 where
  h' =
    hash @ByteString @SHA256
      $  "fixed:out:"
      <> encodeUtf8 (algoName @hashAlgo)
      <> (if recursive then ":r:" else ":")
      <> encodeUtf8 (encodeDigestWith Base16 h)
      <> ":"

computeStorePathForText
  :: StoreDir -> StorePathName -> ByteString -> (StorePathSet -> StorePath)
computeStorePathForText storeDir nm = makeTextPath storeDir nm . hash

computeStorePathForPath
  :: StoreDir
  -> StorePathName        -- ^ Name part of the newly created `StorePath`
  -> FilePath             -- ^ Local `FilePath` to add
  -> Bool                 -- ^ Add target directory recursively
  -> (FilePath -> Bool)   -- ^ Path filter function
  -> Bool                 -- ^ Only used by local store backend
  -> IO StorePath
computeStorePathForPath storeDir name pth recursive _pathFilter _repair = do
  selectedHash <- if recursive then recursiveContentHash else flatContentHash
  pure $ makeFixedOutputPath storeDir recursive selectedHash name
 where
  recursiveContentHash :: IO (Digest SHA256)
  recursiveContentHash = hashFinalize <$> execStateT streamNarUpdate (hashInit @SHA256)
  streamNarUpdate :: StateT (Context SHA256) IO ()
  streamNarUpdate = streamNarIO narEffectsIO pth (modify . flip (hashUpdate @ByteString @SHA256))

  flatContentHash :: IO (Digest SHA256)
  flatContentHash = hashlazy <$> narReadFile narEffectsIO pth
