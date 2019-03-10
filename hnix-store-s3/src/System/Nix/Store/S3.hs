{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module System.Nix.Store.S3 (
  addToStore
) where

import           Codec.Compression.Lzma
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Binary.Put          as B
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.String              (fromString)
import           Data.Text
import qualified Data.Text.Encoding       as E
import           Network.AWS
import           Network.AWS.S3
import           System.Nix.Hash
import           System.Nix.Internal.Hash (digestText32)
import           System.Nix.Nar
import           System.Nix.Path
import           System.Nix.ReadonlyStore

type RepairFlag = Bool
type PathFilter = Path -> Bool

type MonadStore a = ExceptT String AWS a

upsertFile
  :: BucketName
  -> Text
  -> BS.ByteString
  -> Text
  -> AWS PutObjectResponse
upsertFile bucketName pth d mimeType =
  send (putObject bucketName (ObjectKey pth) (toBody d) & poContentType ?~ mimeType)

data NarInfo
  = NarInfo Path BS.ByteString BS.ByteString (Digest 'SHA256) Int (Digest 'SHA256) Int

narInfoToString
  :: Text
  -> NarInfo
  -> BS.ByteString
narInfoToString storeDir (NarInfo pth url compression fileHash fileSize narHash' narSize') =
  BS.unlines
    [ "StorePath: " <> E.encodeUtf8 (pathToText storeDir pth)
    , "URL: " <> url
    , "Compression: " <> compression
    , "FileHash: " <> E.encodeUtf8 (digestText32 fileHash)
    , "FileSize: " <> fromString (show fileSize)
    , "NarHash: " <> E.encodeUtf8 (digestText32 narHash')
    , "NarSize: " <> fromString (show narSize')
    , "References: "
    ]

narInfoFileFor
  :: Text
  -> Path
  -> Text
narInfoFileFor storeDir pth =
  pathToText storeDir pth <> ".narinfo"

addToStore
  :: Text
  -> BucketName
  -> LBS.ByteString
  -> FilePath
  -> Bool
  -> PathFilter
  -> RepairFlag
  -> MonadStore Path
addToStore storeDir bucketName name pth recursive pfilter repair = do
  let
    dump = LBS.toStrict . B.runPut . putNar
    dumpString s = dump . Nar $ Regular NonExecutable (fromIntegral $ LBS.length s) s
    dumpPath = liftIO . (dump <$>) . localPackNar narEffectsIO

  (nar, h) <-
    if recursive
    then do
      bs <- dumpPath pth
      let h = hash @'SHA256 bs
      pure (bs, h)
    else do
      s <- liftIO $ LBS.readFile pth
      let
        bs = dumpString s
        h = hash @'SHA256 bs
      pure (bs, h)

  let
    pth' = makeFixedOutputPath storeDir recursive h . E.decodeUtf8 $ LBS.toStrict name
    narCompressed = nar & lazy %~ compress
    url = "nar/" <> printAsBase32 fileHash <> ".nar.xz"
    fileHash = hash @'SHA256 narCompressed
    narInfo = NarInfo pth' (E.encodeUtf8 url) "xz" fileHash (BS.length narCompressed) (hash @'SHA256 nar) (BS.length nar)

  void . lift $ upsertFile bucketName url narCompressed "application/x-nix-nar"
  void . lift $ upsertFile bucketName (narInfoFileFor storeDir pth') (narInfoToString storeDir narInfo) "text/x-nix-narinfo"

  pure pth'
