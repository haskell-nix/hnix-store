{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.ValidPath
  ( ValidPath(..)
  ) where

import           System.Nix.Hash           (Digest(..))
import           System.Nix.StorePath      (StorePath(..), StorePathSet, KnownStoreDir)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.Hashable             (Hashable (..), hashPtrWithSalt)
import           Data.HashMap.Strict       (HashMap)
import           Data.HashSet              (HashSet)
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)
import qualified Data.Text                 as T
import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           Text.Regex.Base.RegexLike (makeRegex, matchTest)
import           Text.Regex.TDFA.Text      (Regex)

-- | Information about @Path@
data (KnownStoreDir a) => ValidPath a = ValidPath
  { -- | Path itself
    path             :: !(StorePath a)
  , -- | The .drv which led to this 'Path'.
    deriver          :: !(Maybe (StorePath a))
  , -- | NAR hash
    narHash          :: !Text
  , -- | The references of the 'Path'
    references       :: !(StorePathSet a)
  , -- | Registration time
    registrationTime :: !UTCTime
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSize          :: !Integer
  , -- | Whether the path is ultimately trusted, that is, it's a
    -- derivation output that was built locally.
    ultimate         :: !Bool
  , -- | Signatures
    sigs             :: ![Text]
  , -- | Content-addressed
    -- Store path is computed from a cryptographic hash
    -- of the contents of the path, plus some other bits of data like
    -- the "name" part of the path.
    --
    -- ‘ca’ has one of the following forms:
    -- * ‘text:sha256:<sha256 hash of file contents>’ (paths by makeTextPath() / addTextToStore())
    -- * ‘fixed:<r?>:<ht>:<h>’ (paths by makeFixedOutputPath() / addToStore())
    ca               :: !Text
  } deriving (Eq, Ord) -- , Show)
