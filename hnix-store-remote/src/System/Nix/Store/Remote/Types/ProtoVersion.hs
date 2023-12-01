module System.Nix.Store.Remote.Types.ProtoVersion
  ( ProtoVersion(..)
  , HasProtoVersion(..)
  , ourProtoVersion
  ) where

import Data.Word (Word8, Word16)
import GHC.Generics

data ProtoVersion = ProtoVersion
  { protoVersion_major :: Word16
  , protoVersion_minor :: Word8
  }
  deriving (Eq, Generic, Ord, Show)

class HasProtoVersion r where
  hasProtoVersion :: r -> ProtoVersion

instance HasProtoVersion ProtoVersion where
  hasProtoVersion = id

-- | The protocol version we support
ourProtoVersion :: ProtoVersion
ourProtoVersion = ProtoVersion
  { protoVersion_major = 1
  , protoVersion_minor = 21
  }
