{-|
Description : Interact with a binary Nix store.
Maintainer  : Brian McKenna <brian@brianmckenna.org>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.Store.Binary (
  BinaryStoreEffects(..)
, addToStore
) where

import Codec.Compression.Lzma (compress)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Nix.Hash
import System.Nix.Nar
import System.Nix.StorePath
import System.Nix.StorePathMetadata
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as E
import System.Nix.ReadonlyStore

data BinaryStoreEffects m =
  BinaryStoreEffects
    { upsertFile :: Text -> BS.ByteString -> Text -> m ()
    }

narInfoFileFor
  :: StorePath storeDir
  -> Text
narInfoFileFor p =
  encodeBase32 (storePathHash p) <> ".narinfo"

addToStore
  :: forall storeDir m. (MonadIO m, KnownStoreDir storeDir)
  => StorePathName
  -> FilePath
  -> NarHashMode
  -> BinaryStoreEffects m
  -> m (StorePath storeDir)
addToStore name pth narHashMode bse = do
  let
    dump = LBS.toStrict . B.runPut . putNar
    dumpString s = dump . Nar $ Regular NonExecutable (fromIntegral $ LBS.length s) s
    dumpPath = liftIO . (dump <$>) . localPackNar narEffectsIO

  (nar, h) <-
    case narHashMode of
      Recursive -> do
        bs <- dumpPath pth
        let h = hash @'SHA256 bs
        pure (bs, h)
      RegularFile -> do
        s <- liftIO $ LBS.readFile pth
        let
          bs = dumpString s
          h = hash @'SHA256 bs
        pure (bs, h)

  let
    pth' = makeFixedOutputPath @storeDir narHashMode h name
    narCompressed = LBS.toStrict . compress $ LBS.fromStrict nar
    fileHash' = hash @'SHA256 narCompressed
    url = "nar/" <> encodeBase32 fileHash' <> ".nar.xz"
    storePathMetadata =
      StorePathMetadata
        pth'
        Nothing
        (SomeDigest $ hash @'SHA256 nar)
        HS.empty
        (posixSecondsToUTCTime 0)
        (Just . fromIntegral $ BS.length nar)
        BuiltElsewhere
        mempty
        (Just . Fixed narHashMode $ SomeDigest h)
    narInfo =
      NarInfo
        url
        "xz"
        (SomeDigest fileHash')
        (Just . fromIntegral $ BS.length narCompressed)

  upsertFile bse url narCompressed "application/x-nix-nar"
  upsertFile bse (narInfoFileFor pth') (E.encodeUtf8 $ encodeNarInfo storePathMetadata narInfo) "text/x-nix-narinfo"

  pure pth'
