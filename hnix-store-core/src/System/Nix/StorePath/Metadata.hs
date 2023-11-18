{-|
Description : Metadata about Nix store paths.
-}
module System.Nix.StorePath.Metadata
  ( Metadata(..)
  , StorePathTrust(..)
  ) where

import Crypto.Hash (Digest)
import Data.Dependent.Sum (DSum)
import Data.HashSet (HashSet)
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

import System.Nix.Hash (HashAlgo)
import System.Nix.Signature (NarSignature)
import System.Nix.ContentAddress (ContentAddress)

-- | How much do we trust the path, based on its provenance?
-- This is called `Ultimate` in Nix, where Ultimate = True
-- means that the path is ultimately trusted, which
-- corresponds to our @BuiltLocally@
data StorePathTrust
  = -- | It was built locally and thus ultimately trusted
    BuiltLocally
  | -- | It was built elsewhere (and substituted or similar) and so
    -- is less trusted
    BuiltElsewhere
  deriving (Eq, Enum, Generic, Ord, Show)

-- | Metadata (typically about a 'StorePath')
-- This type corresponds to Nix-es `ValidPathInfo`
data Metadata a = Metadata
  { -- | The path this metadata is about
    path :: !a
  , -- | The path to the derivation file that built this path, if any
    -- and known.
    deriverPath :: !(Maybe a)
  , -- TODO should this be optional?
    -- | The hash of the nar serialization of the path.
    narHash :: !(DSum HashAlgo Digest)
  , -- | The paths that this path directly references
    references :: !(HashSet a)
  , -- | When was this path registered valid in the store?
    registrationTime :: !UTCTime
  , -- | The size of the nar serialization of the path, in bytes.
    narBytes :: !(Maybe Word64)
  , -- | How much we trust this path. Nix-es ultimate
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
    contentAddress :: !(Maybe ContentAddress)
  } deriving (Eq, Generic, Ord, Show)
