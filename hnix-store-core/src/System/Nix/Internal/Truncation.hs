{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}

module System.Nix.Internal.Truncation
  ( truncateInNixWay
  )
where

import qualified Data.ByteString        as Bytes

-- | Bytewise truncation of a 'Digest'.
--
-- When truncation length is greater than the length of the bytestring
-- but less than twice the bytestring length, truncation splits the
-- bytestring into a head part (truncation length) and tail part
-- (leftover part), right-pads the leftovers with 0 to the truncation
-- length, and combines the two strings bytewise with 'xor'.
truncateInNixWay
  :: Int -> Bytes.ByteString -> Bytes.ByteString
--  2021-06-07: NOTE: Renamed function, since truncation can be done in a lot of ways, there is no practice of truncting hashes this way, moreover:
-- 1. <https://crypto.stackexchange.com/questions/56337/strength-of-hash-obtained-by-xor-of-parts-of-sha3>
-- 2. <https://www.reddit.com/r/crypto/comments/6olqfm/ways_to_truncated_hash/>
truncateInNixWay n c =
    Bytes.pack $ fmap truncOutputByte [0 .. n-1]
  where

    truncOutputByte :: Int -> Word8
    truncOutputByte i = foldl' (aux i) 0 [0 .. Bytes.length c - 1]

    inputByte :: Int -> Word8
    inputByte j = Bytes.index c j

    aux :: Int -> Word8 -> Int -> Word8
    aux i x j =
      bool
        id
        (`xor` inputByte j)
        (j `mod` n == i)
        x
