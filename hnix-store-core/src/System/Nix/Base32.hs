{-|
Description: Implementation of Nix's base32 encoding.
-}
module System.Nix.Base32 where

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Vector            as V

-- | Encode a 'BS.ByteString' in Nix's base32 encoding
encode :: BS.ByteString -> T.Text
encode c = T.pack $ concatMap char32 [nChar - 1, nChar - 2 .. 0]
  where
    digits32 = V.fromList "0123456789abcdfghijklmnpqrsvwxyz"
    -- The base32 encoding is 8/5's as long as the base256 digest.  This `+ 1`
    -- `- 1` business is a bit odd, but has always been used in C++ since the
    -- base32 truncation was added in was first added in
    -- d58a11e019813902b6c4547ca61a127938b2cc20.
    nChar = fromIntegral $ ((BS.length c * 8 - 1) `div` 5) + 1

    char32 :: Integer -> [Char]
    char32 i = [digits32 V.! digitInd]
      where
        byte j   = BS.index c (fromIntegral j)
        fromIntegral' :: Num b => Integer -> b
        fromIntegral' = fromIntegral
        digitInd = fromIntegral' $
                   sum [fromIntegral (byte j) * (256^j)
                       | j <- [0 .. BS.length c - 1]]
                   `div` (32^i)
                   `mod` 32
