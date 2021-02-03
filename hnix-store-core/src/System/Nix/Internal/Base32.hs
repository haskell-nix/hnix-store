module System.Nix.Internal.Base32 where


import           Data.Bool                      ( bool )
import           Data.Maybe                     ( fromMaybe )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as Bytes
import qualified Data.ByteString.Char8         as Bytes.Char8
import qualified Data.Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import           Data.Text                      ( Text )
import           Data.Bits                      ( shiftR )
import           Data.Word                      ( Word8 )
import           Data.List                      ( unfoldr )
import           Numeric                        ( readInt )


-- omitted: E O U T
digits32 :: Vector Char
digits32 = Vector.fromList "0123456789abcdfghijklmnpqrsvwxyz"

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
  nChar = fromIntegral $ ((Bytes.length c * 8 - 1) `div` 5) + 1

  byte  = Bytes.index c . fromIntegral

  -- May need to switch to a more efficient calculation at some
  -- point.
  bAsInteger :: Integer
  bAsInteger =
    sum
      [ fromIntegral (byte j) * (256 ^ j)
        | j <- [0 .. Bytes.length c - 1] ]

  char32 :: Integer -> Char
  char32 i = digits32 Vector.! digitInd
   where
    digitInd =
      fromIntegral $
        bAsInteger `div` (32^i) `mod` 32

-- | Decode Nix's base32 encoded text
decode :: Text -> Either String ByteString
decode what =
  bool
    (Left "Invalid Base32 string")
    (unsafeDecode what)
    (Data.Text.all (`elem` digits32) what)

-- | Decode Nix's base32 encoded text
-- Doesn't check if all elements match `digits32`
unsafeDecode :: Text -> Either String ByteString
unsafeDecode what =
  case
      readInt
        32
        (`elem` digits32)
        (\c -> fromMaybe (error "character not in digits32")
          $ Vector.findIndex (== c) digits32
        )
        (Data.Text.unpack what)
    of
      [(i, _)] -> Right $ padded $ integerToBS i
      x        -> Left $ "Can't decode: readInt returned " <> show x
 where
  padded x
    | Bytes.length x < decLen = x `Bytes.append` bstr
    | otherwise               = x
   where
    bstr = Bytes.Char8.pack $ take (decLen - Bytes.length x) (cycle "\NUL")

  decLen = Data.Text.length what * 5 `div` 8

-- | Encode an Integer to a bytestring
-- Similar to Data.Base32String (integerToBS) without `reverse`
integerToBS :: Integer -> ByteString
integerToBS 0 = Bytes.pack [0]
integerToBS i
    | i > 0     = Bytes.pack $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)
