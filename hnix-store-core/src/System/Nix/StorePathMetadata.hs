{-|
Description : Metadata about Nix store paths.
-}
module System.Nix.StorePathMetadata where

import System.Nix.StorePath (StorePath, StorePathSet, ContentAddressableAddress)
import System.Nix.Hash (SomeNamedDigest)
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Word (Word64)
import System.Nix.Signature (NarSignature)

-- | Metadata about a 'StorePath'
data StorePathMetadata = StorePathMetadata
  { -- | The path this metadata is about
    path :: !StorePath
  , -- | The path to the derivation file that built this path, if any
    -- and known.
    deriverPath :: !(Maybe StorePath)
  , -- TODO should this be optional?
    -- | The hash of the nar serialization of the path.
    narHash :: !SomeNamedDigest
  , -- | The paths that this path directly references
    references :: !StorePathSet
  , -- | When was this path registered valid in the store?
    registrationTime :: !UTCTime
  , -- | The size of the nar serialization of the path, in bytes.
    narBytes :: !(Maybe Word64)
  , -- | How much we trust this path.
    trust :: !StorePathTrust
  , -- | A set of cryptographic attestations of this path's validity.
    --
    -- There is no guarantee from this type alone that these
    -- signatures are valid.
    sigs :: !(Set NarSignature)
  , -- | Whether and how this store path is content-addressable.
    --
    -- There is no guarantee from this type alone that this address
    -- is actually correct for this store path.
    contentAddressableAddress :: !(Maybe ContentAddressableAddress)
  }

-- | How much do we trust the path, based on its provenance?
data StorePathTrust
  = -- | It was built locally and thus ultimately trusted
    BuiltLocally
  | -- | It was built elsewhere (and substituted or similar) and so
    -- is less trusted
    BuiltElsewhere
  deriving (Show, Eq)
