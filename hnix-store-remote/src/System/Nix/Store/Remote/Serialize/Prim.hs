{-|
Description : Nix-like serialization primitives
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Store.Remote.Serialize.Prim where

import Data.Fixed (Uni)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Putter)
import Data.Time (NominalDiffTime, UTCTime)
import System.Nix.StorePath (StoreDir, StorePath, InvalidPathError)

import qualified Data.HashSet
import qualified Data.Serialize.Get
import qualified Data.Serialize.Put
import qualified Data.ByteString
import qualified Data.Time.Clock.POSIX
import qualified System.Nix.StorePath

-- * Int

-- | Deserialize Nix like integer
getInt :: Get Int
getInt = fromIntegral <$> Data.Serialize.Get.getWord64le

-- | Serialize Nix like integer
putInt :: Putter Int
putInt = Data.Serialize.Put.putWord64le . fromIntegral

-- * Bool

-- | Deserialize @Bool@ from integer
getBool :: Get Bool
getBool = (== 1) <$> (getInt :: Get Int)

-- | Serialize @Bool@ into integer
putBool :: Putter Bool
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)

-- * Enum

-- | Deserialize @Enum@ to integer
getEnum :: Enum a => Get a
getEnum = toEnum <$> getInt

-- | Serialize @Enum@ to integer
putEnum :: Enum a => Putter a
putEnum = putInt . fromEnum

-- * UTCTime

-- | Deserialize @UTCTime@ from integer
-- Only 1 second precision.
getTime :: Get UTCTime
getTime =
  Data.Time.Clock.POSIX.posixSecondsToUTCTime
  . seconds
  <$> getInt
  where
    -- fancy (*10^12), from Int to Uni to Pico(seconds)
    seconds :: Int -> NominalDiffTime
    seconds n = realToFrac (toEnum n :: Uni)

-- | Serialize @UTCTime@ to integer
-- Only 1 second precision.
putTime :: Putter UTCTime
putTime =
  putInt
  . seconds
  . Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds
  where
    -- fancy (`div`10^12), from Pico to Uni to Int
    seconds :: NominalDiffTime -> Int
    seconds = (fromEnum :: Uni -> Int) . realToFrac

-- * Combinators

-- | Deserialize a list
getMany :: Get a -> Get [a]
getMany parser = do
  count <- getInt
  replicateM count parser

-- | Serialize a list
putMany :: Foldable t => Putter a -> Putter (t a)
putMany printer xs = do
  putInt (length xs)
  mapM_ printer xs

-- * ByteString

-- | Deserialize length prefixed string
-- into @ByteString@, checking for correct padding
getByteString :: Get ByteString
getByteString = do
  len <- getInt
  st  <- Data.Serialize.Get.getByteString len
  when (len `mod` 8 /= 0) $ do
    pads <- unpad $ fromIntegral $ 8 - (len `mod` 8)
    unless (all (== 0) pads) $ fail $ "No zeroes" <> show (st, len, pads)
  pure st
  where unpad x = replicateM x Data.Serialize.Get.getWord8

-- | Serialize @ByteString@ using length
-- prefixed string packing with padding to 8 bytes
putByteString :: Putter ByteString
putByteString x = do
  putInt len
  Data.Serialize.Put.putByteString x
  when (len `mod` 8 /= 0) $ pad $ 8 - (len `mod` 8)
 where
  len :: Int
  len = fromIntegral $ Data.ByteString.length x
  pad count = replicateM_ count (Data.Serialize.Put.putWord8 0)

-- | Deserialize a list of @ByteString@s
getByteStrings :: Get [ByteString]
getByteStrings = getMany getByteString

-- | Serialize a list of @ByteString@s
putByteStrings :: Foldable t => Putter (t ByteString)
putByteStrings = putMany putByteString

-- * Text

-- | Deserialize @Text@
getText :: Get Text
getText = decodeUtf8 <$> getByteString

-- | Serialize @Text@
putText :: Putter Text
putText = putByteString . encodeUtf8

-- | Deserialize a list of @Text@s
getTexts :: Get [Text]
getTexts = fmap decodeUtf8 <$> getByteStrings

-- | Serialize a list of @Text@s
putTexts :: (Functor f, Foldable f) => Putter (f Text)
putTexts = putByteStrings . fmap encodeUtf8

-- * StorePath

-- | Deserialize @StorePath@, checking
-- that @StoreDir@ matches expected value
getPath :: StoreDir -> Get (Either InvalidPathError StorePath)
getPath sd =
  System.Nix.StorePath.parsePath sd <$> getByteString

-- | Serialize @StorePath@ with its associated @StoreDir@
putPath :: StoreDir -> Putter StorePath
putPath storeDir =
  putByteString
  . System.Nix.StorePath.storePathToRawFilePath storeDir

-- | Deserialize a @HashSet@ of @StorePath@s
getPaths :: StoreDir -> Get (HashSet (Either InvalidPathError StorePath))
getPaths sd =
  Data.HashSet.fromList
  . fmap (System.Nix.StorePath.parsePath sd)
  <$> getByteStrings

-- | Serialize a @HashSet@ of @StorePath@s
putPaths :: StoreDir -> Putter (HashSet StorePath)
putPaths storeDir =
  putByteStrings
  . Data.HashSet.toList
  . Data.HashSet.map
     (System.Nix.StorePath.storePathToRawFilePath storeDir)
