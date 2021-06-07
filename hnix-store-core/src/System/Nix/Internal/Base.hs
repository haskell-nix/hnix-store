module System.Nix.Internal.Base
  ( module System.Nix.Internal.Base
  , encode
  , decode
  )
where

import System.Nix.Internal.Base32

-- | Constructors to indicate the base encodings
data BaseEncoding
  = Base16
  | NixBase32
  -- | ^ Nix has a special map of Base32 encoding
  | Base64
