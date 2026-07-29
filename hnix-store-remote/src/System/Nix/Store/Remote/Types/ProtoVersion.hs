{-# LANGUAGE OverloadedStrings #-}
module System.Nix.Store.Remote.Types.ProtoVersion
  ( ProtoVersion(..)
  , ProtoFeature(..)
  , protoFeatureToText
  , protoFeatureFromText
  , HasProtoVersion(..)
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

-- | An optional protocol capability, negotiated during handshake.
data ProtoFeature
  = ProtoFeature_RealisationWithPathNotHash
  -- ^ Use StorePath-based realisations instead of hash-based ones.
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | The name of a feature as exchanged on the wire.
protoFeatureToText :: ProtoFeature -> Text
protoFeatureToText = \case
  ProtoFeature_RealisationWithPathNotHash -> "realisation-with-path-not-hash"

protoFeatureFromText :: Text -> Maybe ProtoFeature
protoFeatureFromText t =
  lookup t [ (protoFeatureToText f, f) | f <- [minBound .. maxBound] ]

data ProtoVersion = ProtoVersion
  { protoVersion_major :: Word16
  , protoVersion_minor :: Word8
  , protoVersion_features :: Set ProtoFeature
  }
  deriving (Eq, Generic, Show)

instance PartialOrd ProtoVersion where
  leq a b =
    (protoVersion_major a, protoVersion_minor a)
      <= (protoVersion_major b, protoVersion_minor b)
    && protoVersion_features a `Data.Set.isSubsetOf` protoVersion_features b

-- | Check whether a protocol version includes a given feature.
hasFeature :: ProtoFeature -> ProtoVersion -> Bool
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
    , protoVersion_features = Data.Set.singleton ProtoFeature_RealisationWithPathNotHash
    }

class HasProtoVersion r where
  hasProtoVersion :: r -> ProtoVersion

instance HasProtoVersion ProtoVersion where
  hasProtoVersion = id
