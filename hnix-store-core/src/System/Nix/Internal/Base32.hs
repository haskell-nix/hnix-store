
module System.Nix.Internal.Base32 where

import           Data.ByteString        (ByteString)
import           Data.Bits              (shiftR)
import           Data.List              (unfoldr)
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import           Data.Word              (Word8)
import           Numeric                (readInt)

import qualified Data.Maybe
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Vector

-- Special Nix Base 32 dictinary with omitted: {E,O,U,T}
digits32 :: Vector Char
digits32 = Data.Vector.fromList "0123456789abcdfghijklmnpqrsvwxyz"

-- | Encode a 'BS.ByteString' in Nix's base32 encoding
encode :: ByteString -> Text
encode c = Data.Text.pack $ map char32 [nChar - 1, nChar - 2 .. 0]
  where
    -- Each base32 character gives us 5 bits of information, while
    -- each byte gives is 8. Because 'div' rounds down, we need to add
    -- one extra character to the result, and because of that extra 1
    -- we need to subtract one from the number of bits in the
    -- bytestring to cover for the case where the number of bits is
    -- already a factor of 5. Thus, the + 1 outside of the 'div' and
    -- the - 1 inside of it.
    nChar = fromIntegral $ succ $ (Data.ByteString.length c * 8 - 1) `div` 5

    byte = Data.ByteString.index c . fromIntegral

    -- May need to switch to a more efficient calculation at some
    -- point.
    bAsInteger :: Integer
    bAsInteger = sum [fromIntegral (byte j) * (256 ^ j)
                     | j <- [0 .. Data.ByteString.length c - 1]
                     ]

    char32 :: Integer -> Char
    char32 i = digits32 Data.Vector.! digitInd
      where
        digitInd = fromIntegral $
                   bAsInteger
                   `div` (32^i)
                   `mod` 32

-- | Decode Nix's base32 encoded text
decode :: Text -> Either String ByteString
decode what =
  if Data.Text.all (`elem` digits32) what
    then unsafeDecode what
    else Left "Invalid base32 string"

-- | Decode Nix's base32 encoded text
-- Doesn't check if all elements match `digits32`
unsafeDecode :: Text -> Either String ByteString
unsafeDecode what =
  case readInt 32
         (`elem` digits32)
         (\c -> Data.Maybe.fromMaybe (error "character not in digits32")
                  $ Data.Vector.findIndex (==c) digits32)
         (Data.Text.unpack what)
    of
      [(i, _)] -> Right $ padded $ integerToBS i
      x        -> Left $ "Can't decode: readInt returned " ++ show x
  where
    padded x
      | Data.ByteString.length x < decLen = x `Data.ByteString.append` bstr
      | otherwise = x
     where
      bstr = Data.ByteString.Char8.pack $ take (decLen - Data.ByteString.length x) (cycle "\NUL")

    decLen = Data.Text.length what * 5 `div` 8

-- | Encode an Integer to a bytestring
-- Similar to Data.Base32String (integerToBS) without `reverse`
integerToBS :: Integer -> ByteString
integerToBS 0 = Data.ByteString.pack [0]
integerToBS i
    | i > 0     = Data.ByteString.pack $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)
