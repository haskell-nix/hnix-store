{-# language CPP #-}

module System.Nix.Internal.Base
  ( BaseEncoding(Base16,NixBase32,Base64)
  , encodeWith
  , decodeWith
  )
where

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as Bytes
import qualified Data.ByteString.Base16 as Base16
import qualified System.Nix.Base32      as Base32  -- Nix has own Base32 encoding
import qualified Data.ByteString.Base64 as Base64

-- | Constructors to indicate the base encodings
data BaseEncoding
  = NixBase32
  -- | ^ Nix has a special map of Base32 encoding
  -- Placed first, since it determines Haskell optimizations of pattern matches, & NixBase seems be the most widely used in Nix.
  | Base16
  | Base64


-- | Encode @ByteString@ with @Base@ encoding, produce @Text@.
encodeWith :: BaseEncoding -> Bytes.ByteString -> T.Text
encodeWith Base16 = decodeUtf8 . Base16.encode
encodeWith NixBase32 = Base32.encode
encodeWith Base64 = decodeUtf8 . Base64.encode

-- | Take the input & @Base@ encoding witness -> decode into @Text@.
decodeWith :: BaseEncoding -> T.Text -> Either String Bytes.ByteString
#if MIN_VERSION_base16_bytestring(1,0,0)
decodeWith Base16 = Base16.decode . encodeUtf8
#else
decodeWith Base16 = lDecode  -- this tacit sugar simply makes GHC pleased with number of args
 where
  lDecode t =
    case Base16.decode (encodeUtf8 t) of
      (x, "") -> pure $ x
      _       -> Left $ "Unable to decode base16 string" <> toString t
#endif
decodeWith NixBase32 = Base32.decode
decodeWith Base64 = Base64.decode . encodeUtf8
