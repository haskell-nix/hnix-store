module System.Nix.Base
  ( BaseEncoding(Base16,NixBase32,Base64)
  , encodeWith
  , decodeWith
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Text.Encoding qualified
import Data.ByteString.Base16 qualified
import Data.ByteString.Base64 qualified

import System.Nix.Base32 qualified -- Nix has own Base32 encoding

-- | Constructors to indicate the base encodings
data BaseEncoding
  = NixBase32
  -- | ^ Nix has a special map of Base32 encoding
  -- Placed first, since it determines Haskell optimizations of pattern matches,
  -- & NixBase seems be the most widely used in Nix.
  | Base16
  | Base64
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)

-- | Encode @ByteString@ with @Base@ encoding, produce @Text@.
encodeWith :: BaseEncoding -> ByteString -> Text
encodeWith Base16 =
  Data.Text.Encoding.decodeUtf8
  . Data.ByteString.Base16.encode
encodeWith NixBase32 = System.Nix.Base32.encode
encodeWith Base64 =
  Data.Text.Encoding.decodeUtf8
  . Data.ByteString.Base64.encode

-- | Take the input & @Base@ encoding witness -> decode into @Text@.
decodeWith :: BaseEncoding -> Text -> Either String ByteString
decodeWith Base16 =
  Data.ByteString.Base16.decode
  . Data.Text.Encoding.encodeUtf8
decodeWith NixBase32 =  System.Nix.Base32.decode
decodeWith Base64 =
  Data.ByteString.Base64.decode
  . Data.Text.Encoding.encodeUtf8
