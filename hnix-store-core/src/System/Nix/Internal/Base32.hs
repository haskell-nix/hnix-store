{-# LANGUAGE OverloadedStrings   #-}

-- | Nix has Nix-specific Base32 encoding.
-- Stick to using the Text. But lib provides ByteString (Bytes) just for unsafe optimization case.

module System.Nix.Internal.Base32 where


import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Text.Builder
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as Bytes
import qualified Data.ByteString.Char8  as Bytes.Char8
import qualified Data.ByteString.Internal as Bytes (c2w, unpackChars)
import           Data.Bits              (shiftR)
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector
import           Data.Word              (Word8)
import           Numeric                (readInt)




-- Special Nix Base 32 dictinary with omitted: {E,O,U,T}
dictNixBase32 :: Vector Char
dictNixBase32 = Vector.fromList "0123456789abcdfghijklmnpqrsvwxyz"
dictNixBase32Bytes :: Vector Word8
dictNixBase32Bytes = Vector.fromList (fmap Bytes.c2w "0123456789abcdfghijklmnpqrsvwxyz")

type NixBase32 = Text
type NixBase32Bytes = ByteString

-- | Encode a Text in Nix-specific Base32 encoding
encode :: Text -> NixBase32
encode text = Text.pack $ fmap char32 [nChar - 1, nChar - 2 .. 0]
  where
    -- Each base32 character gives us 5 bits of information, while
    -- each byte gives is 8. Because 'div' rounds down, we need to add
    -- one extra character to the result, and because of that extra 1
    -- we need to subtract one from the number of bits in the
    -- bytestring to cover for the case where the number of bits is
    -- already a factor of 5. Thus, the + 1 outside of the 'div' and
    -- the - 1 inside of it.
    nChar = fromIntegral $ succ $ (Text.length text * 8 - 1) `div` 5

    char = Text.index text . fromIntegral

    -- May need to switch to a more efficient calculation at some
    -- point.
    bAsInteger :: Integer
    bAsInteger = sum [fromIntegral (char j) * (256 ^ j)
                     | j <- [0 .. Text.length text - 1]
                     ]

    char32 :: Integer -> Char
    char32 i = dictNixBase32 Vector.! digitInd
      where
        digitInd = fromIntegral $
                   bAsInteger
                   `div` (32^i)
                   `mod` 32

-- | Encode a ByteString in Nix-specific Base32 encoding
encodeBytes :: ByteString -> NixBase32Bytes
encodeBytes bytes = Bytes.pack $ fmap char32 [nChar - 1, nChar - 2 .. 0]
  where
    -- Each base32 character gives us 5 bits of information, while
    -- each byte gives is 8. Because 'div' rounds down, we need to add
    -- one extra character to the result, and because of that extra 1
    -- we need to subtract one from the number of bits in the
    -- bytestring to cover for the case where the number of bits is
    -- already a factor of 5. Thus, the + 1 outside of the 'div' and
    -- the - 1 inside of it.
    nChar = fromIntegral $ succ $ (Bytes.length bytes * 8 - 1) `div` 5

    char32 :: Integer -> Word8
    char32 i = dictNixBase32Bytes Vector.! digitInd
      where
        digitInd = fromIntegral $
                   bAsInteger
                   `div` (32^i)
                   `mod` 32

    -- May need to switch to a more efficient calculation at some
    -- point.
    bAsInteger :: Integer
    bAsInteger = sum [fromIntegral (byte j) * (256 ^ j)
                     | j <- [0 .. Bytes.length bytes - 1]
                     ]
    byte = Bytes.index bytes . fromIntegral



-- | Decode from Nix-specific Base32 encoding.
decode :: NixBase32 -> Either String Text
decode what =
  if Text.all (`elem` dictNixBase32) what
    then unsafeDecode what
    else Left "Invalid Nix-Base32 string"

-- | Decode from Nix-specific Base32 encoding.
decodeToBytes :: NixBase32Bytes -> Either String ByteString
decodeToBytes what =
  if Bytes.all (`elem` dictNixBase32Bytes) what
    then unsafeDecodeToBytes what
    else Left "Invalid Nix-Base32 string"

-- | Fast decoding from Nix-specific Base32 encoding.
-- Doesn't check if all elements match `dictNixBase32`.
unsafeDecode :: NixBase32 -> Either String Text
unsafeDecode what =
  -- 2021-01-31: NOTE: `text-show` has a faster version of this implementation,
  -- because here Text -> String happens for readInt.
  case readIntegerIntoString $ Text.unpack what of
    [(i, _)] -> Right $ padded $ integerToText i
    x        -> Left $ "Can't decode `readInt` returned : " <> show x
 where
  readIntegerIntoString :: (ReadS Integer)
  readIntegerIntoString =
    readInt
      32
      (`elem` dictNixBase32)
      (\ c -> Data.Maybe.fromMaybe
        (error "character not in dictNixBase32")
        $ Vector.findIndex (== c) dictNixBase32)

  padded :: (Text -> Text)
  padded x
    | Text.length x < decLen = x <> bstr
    | otherwise = x
   where
    bstr = Text.pack $ take (decLen - Text.length x) (cycle "\NUL")
    decLen = Text.length what * 5 `div` 8

-- | Fast decoding from Nix-specific Base32 encoding.
-- Doesn't check if all elements match `dictNixBase32`.
unsafeDecodeToBytes :: NixBase32Bytes -> Either String ByteString
unsafeDecodeToBytes what =
  case readIntegerIntoString $ Bytes.unpackChars what of
      [(i, _)] -> Right $ padded $ integerToBytes i
      x        -> Left $ "Can't decode `readInt` returned : " <> show x
 where
  readIntegerIntoString :: (ReadS Integer)
  readIntegerIntoString =
    readInt
      32
      -- 2021-01-30: NOTE: Use of `readInt` that requires `Char -> Bytestring` convertion a bit defeats a ByteString pipeline.
      -- Using dictNixBase32 with Char because of it
      (`elem` dictNixBase32)
      (\ c -> Data.Maybe.fromMaybe
        (error "character not in dictNixBase32")
        $ Vector.findIndex (== c) dictNixBase32)
  padded x
    | Bytes.length x < decLen = x <> bstr
    | otherwise = x
   where
    bstr = Bytes.Char8.pack $ take (decLen - Bytes.length x) (cycle "\NUL")
    decLen = Bytes.length what * 5 `div` 8


-- | Encode an Integer to a Text
integerToText :: Integer -> Text
integerToText = Text.Builder.run . Text.Builder.decimal


-- | Encode an Integer to a ByteString
-- Similar to Data.Base32String (integerToBS) without `reverse`
integerToBytes :: Integer -> ByteString
integerToBytes i
  | i > 0     = Bytes.unfoldr f i
  | i == 0     = Bytes.empty
  | otherwise = error "integerToBytes not defined for negative values."
 where
  f 0 = Nothing
  f x = Just (fromInteger x :: Word8, x `shiftR` 8)
