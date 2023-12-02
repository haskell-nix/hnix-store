module System.Nix.Store.Remote.Types.Query.Missing
  ( Missing(..)
  ) where

import Data.HashSet (HashSet)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Nix.StorePath (StorePath)

-- | Result of @QueryMissing@ @StoreRequest@
data Missing = Missing
  { missingWillBuild :: HashSet StorePath -- ^ Paths that will be built
  , missingWillSubstitute :: HashSet StorePath -- ^ Paths that can be substituted from cache
  , missingUnknownPaths :: HashSet StorePath -- ^ Path w/o any information
  , missingDownloadSize :: Word64 -- ^ Total size of packed NARs to download
  , missingNarSize :: Word64 -- ^ Total size of NARs after unpacking
  }
  deriving (Eq, Generic, Ord, Show)
