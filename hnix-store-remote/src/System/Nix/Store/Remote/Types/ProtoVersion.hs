{-# LANGUAGE OverloadedStrings #-}
module System.Nix.Store.Remote.Types.ProtoVersion
  ( ProtoVersion(..)
  , HasProtoVersion(..)
  , featureRealisationWithPath
  , hasFeature
  , minVersionNumber
  ) where

import Algebra.PartialOrd (PartialOrd(..))
import Data.Default.Class (Default(def))
import Data.Set (Set)
import Data.Set qualified
import Data.Text (Text)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)

data ProtoVersion = ProtoVersion
  { protoVersion_major :: Word16
  , protoVersion_minor :: Word8
  , protoVersion_features :: Set Text
  }
  deriving (Eq, Generic, Show)

instance PartialOrd ProtoVersion where
  leq a b =
    (protoVersion_major a, protoVersion_minor a)
      <= (protoVersion_major b, protoVersion_minor b)
    && protoVersion_features a `Data.Set.isSubsetOf` protoVersion_features b

-- | The feature for using StorePath-based realisations
-- instead of hash-based ones.
featureRealisationWithPath :: Text
featureRealisationWithPath = "realisation-with-path-not-hash"

-- | Check whether a protocol version includes a given feature.
hasFeature :: Text -> ProtoVersion -> Bool
hasFeature f = Data.Set.member f . protoVersion_features

-- | Take the minimum by version number, with empty features.
-- Used in handshake before feature negotiation.
minVersionNumber :: ProtoVersion -> ProtoVersion -> ProtoVersion
minVersionNumber a b =
  let (major', minor') = min
        (protoVersion_major a, protoVersion_minor a)
        (protoVersion_major b, protoVersion_minor b)
  in ProtoVersion major' minor' mempty

-- | The protocol version we support
instance Default ProtoVersion where
  def = ProtoVersion
    { protoVersion_major = 1
    , protoVersion_minor = 38
    , protoVersion_features = Data.Set.singleton featureRealisationWithPath
    }

class HasProtoVersion r where
  hasProtoVersion :: r -> ProtoVersion

instance HasProtoVersion ProtoVersion where
  hasProtoVersion = id
