{-|
Description : Utilities for packing stuff
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Util where

import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

putInt :: Integral a => a -> Put
putInt = putWord64le . fromIntegral

getInt :: Integral a => Get a
getInt = fromIntegral <$> getWord64le

-- length prefixed string packing with padding to 8 bytes
putByteStringLen :: BS.ByteString -> Put
putByteStringLen x = do
  putInt $ len
  putByteString x
  pad $ 8 - (len `mod` 8)
  where len = BS.length x
        pad x = replicateM_ x (putWord8 0)

putByteStrings :: Foldable t => t BS.ByteString -> Put
putByteStrings xs = do
  putInt $ length xs
  mapM_ putByteStringLen xs

getByteStringLen :: Get LBS.ByteString
getByteStringLen = do
  len <- getInt
  st <- getLazyByteString len
  when (len `mod` 8 /= 0) $ do
    pads <- unpad $ fromIntegral $ 8 - (len `mod` 8)
    unless (all (==0) pads) $ fail $ "No zeroes" ++ show (st, len, pads)
  return st
  where unpad x = replicateM x getWord8

getByteStrings :: Get [LBS.ByteString]
getByteStrings = do
  count <- getInt
  res <- sequence $ replicate count getByteStringLen
  return res
