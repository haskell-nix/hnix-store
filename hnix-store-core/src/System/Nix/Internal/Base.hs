{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP #-}

module System.Nix.Internal.Base where


import qualified Data.ByteString.Base16 as Bytes.Base16
import qualified System.Nix.Base32      as Bytes.Base32  -- Nix has own Base32 encoding
import qualified Data.ByteString.Base64 as Bytes.Base64
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           Data.Coerce            (coerce)
import qualified System.Nix.Internal.Old as Old


-- | Constructors to indicate the base encodings
data BaseEncoding
  = Base16
  | Base32
  -- ^ Nix has a special map of Base32 encoding
  | Base64


-- | Take BaseEncoding type of the output -> take the Digeest as input -> encode Digest
encodeInBytes :: BaseEncoding -> Old.Digest a -> Text
encodeInBytes Base16 = Text.decodeUtf8 . Bytes.Base16.encode . coerce
encodeInBytes Base32 = Text.decodeUtf8 . Bytes.Base32.encode . coerce
encodeInBytes Base64 = Text.decodeUtf8 . Bytes.Base64.encode . coerce


-- | Take BaseEncoding type of the input -> take the input itself -> decodeBase into Digest
decodeBase :: BaseEncoding -> Text -> Either String (Old.Digest a)
#if MIN_VERSION_base16_bytestring(1,0,0)
decodeBase Base16 = fmap Old.Digest . Bytes.Base16.decode . Text.encodeUtf8
#else
decodeBase Base16 = lDecode  -- this tacit sugar simply makes GHC pleased with number of args
 where
  lDecode t = case Base16.decode (Text.encodeUtf8 t) of
    (x, "") -> Right $ Old.Digest x
    _       -> Left $ "Unable to decode base16 string" <> Text.unpack t
#endif
decodeBase Base32 = fmap Old.Digest . Bytes.Base32.decode . Text.encodeUtf8
decodeBase Base64 = fmap Old.Digest . Bytes.Base64.decode . Text.encodeUtf8
