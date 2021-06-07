module System.Nix.Internal.Base
  ( module System.Nix.Internal.Base
  , Base32.encode
  , Base32.decode
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
encodeWith Base16 = T.decodeUtf8 . Base16.encode
encodeWith NixBase32 = Base32.encode
encodeWith Base64 = T.decodeUtf8 . Base64.encode
