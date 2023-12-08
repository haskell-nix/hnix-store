module System.Nix.Store.Remote.Types.ProtoVersion
  ( ProtoVersion(..)
  , HasProtoVersion(..)
  ) where

import Data.Default.Class (Default(def))
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)

data ProtoVersion = ProtoVersion
  { protoVersion_major :: Word16
  , protoVersion_minor :: Word8
  }
  deriving (Eq, Generic, Ord, Show)

-- | The protocol version we support
instance Default ProtoVersion where
  def = ProtoVersion
    { protoVersion_major = 1
    , protoVersion_minor = 24
    }

class HasProtoVersion r where
  hasProtoVersion :: r -> ProtoVersion

instance HasProtoVersion ProtoVersion where
  hasProtoVersion = id
