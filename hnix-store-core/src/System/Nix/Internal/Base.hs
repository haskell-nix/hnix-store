{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP #-}

module System.Nix.Internal.Base where


import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Coerce            (coerce)
import qualified System.Nix.Internal.Old as O


-- | Constructors to indicate the base encodings
data BaseEncoding
  = Base16
  | Base32
  -- ^ Nix has a special map of Base32 encoding
  | Base64


-- | Take BaseEncoding type of the output -> take the Digeest as input -> encode O.Digest
encodeInBase :: BaseEncoding -> O.Digest a -> T.Text
encodeInBase Base16 = T.decodeUtf8 . Base16.encode . coerce
encodeInBase Base32 = Base32.encode . coerce
encodeInBase Base64 = T.decodeUtf8 . Base64.encode . coerce


-- | Take BaseEncoding type of the input -> take the input itself -> decodeBase into O.Digest
decodeBase :: BaseEncoding -> T.Text -> Either String (O.Digest a)
#if MIN_VERSION_base16_bytestring(1,0,0)
decodeBase Base16 = fmap O.Digest . Base16.decode . T.encodeUtf8
#else
decodeBase Base16 = lDecode  -- this tacit sugar simply makes GHC pleased with number of args
 where
  lDecode t = case Base16.decode (T.encodeUtf8 t) of
    (x, "") -> Right $ O.Digest x
    _       -> Left $ "Unable to decode base16 string" <> T.unpack t
#endif
decodeBase Base32 = fmap O.Digest . Base32.decode
decodeBase Base64 = fmap O.Digest . Base64.decode . T.encodeUtf8
