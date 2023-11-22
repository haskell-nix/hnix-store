module System.Nix.Store.Remote.Types.ProtoVersion
  ( ProtoVersion(..)
  , HasProtoVersion(..)
  ) where

import Data.Word (Word8, Word16)

data ProtoVersion = ProtoVersion
  { protoVersion_major :: Word16
  , protoVersion_minor :: Word8
  }
  deriving (Eq, Ord, Show)

class HasProtoVersion r where
  protoVersion :: r -> ProtoVersion
