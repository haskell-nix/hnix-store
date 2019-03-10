{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ExistentialQuantification  #-}
module System.Nix.Path where

import Data.Word
import           GHC.TypeLits
import           System.Nix.Hash
import           Data.Time
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.Hashable             (Hashable (..), hashPtrWithSalt)
import           Data.HashMap.Strict       (HashMap)
import           Data.HashSet              (HashSet)
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           Text.Regex.Base.RegexLike (makeRegex, matchTest)
import           Text.Regex.TDFA.Text      (Regex)

-- | The hash algorithm used for store path hashes.
type PathHashAlgo = 'Truncated 20 'SHA256

-- | The name portion of a Nix path.
--
-- Must be composed of a-z, A-Z, 0-9, +, -, ., _, ?, and =, can't
-- start with a ., and must have at least one character.
newtype PathName = PathName
  { pathNameContents :: Text -- ^ The contents of the path name
  } deriving (Eq, Ord, Show, Hashable)

-- | A regular expression for matching a valid 'PathName'
nameRegex :: Regex
nameRegex =
  makeRegex "[a-zA-Z0-9\\+\\-\\_\\?\\=][a-zA-Z0-9\\+\\-\\.\\_\\?\\=]*"

-- | Construct a 'PathName', assuming the provided contents are valid.
pathName :: Text -> Maybe PathName
pathName n = case matchTest nameRegex n of
  True  -> Just $ PathName n
  False -> Nothing

-- | A path in a store.
--
-- @root@: The root path of the store (e.g. "/nix/store").
data Path (root :: Symbol) = Path !(Digest PathHashAlgo) !PathName
  deriving (Eq, Ord, Show)

type PathSet root = HashSet (Path root)

-- | Metadata about a valid @Path@ in the store.
data PathInfo store = PathInfo
  { -- | Path itself
    path             :: !(Path store)
  , -- | The .drv which led to this 'Path'.
    deriver        :: !(Maybe (Path store))
  , -- | The hash of the serialization of this path.
    narHash          :: !NamedDigest
  , -- | The references of the 'Path'.
    references     :: !(PathSet store)
  , -- | When this store path was registered valid.
    registrationTime :: !UTCTime
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSizeVP        :: !Word64
  , -- | Whether the path is ultimately trusted, that is, it's a
    -- derivation output that was built locally.
    ultimate         :: !Bool
  , -- | Signatures attesting to the validity of this registration.
    sigs             :: ![Text] -- TODO better type?
  , -- | Whether or not the store path is content-addressed, and if so
    ca               :: !(Maybe ContentAddressedHash)
  }


-- | The different types of content-addressed hashing we have in Nix.
data ContentAddressedHash
  = RegularFile (Digest SHA256)
    -- ^ A regular file hashed like sha256sum.
  | forall algo . NamedAlgorithm algo =>
      FixedFile (HashMode algo) (Digest algo)
      -- ^ A file hashed via the add-fixed-file-to-store approach.
      -- This can in fact overlap with RegularFile (if the 'HashMode'
      -- is 'Flat @SHA256'), but the resulting Nix store hash is
      -- different for stupid legacy reasons.

-- | A specification of how to hash a file.
data HashMode (a :: HashAlgorithm)
  = Flat -- ^ Normal hashing of a regular file.
  | Recursive -- ^ Hashing of a serialization of a file, compatible
              -- with directories and executable files as well as
              -- regular files.

instance Hashable (Path store) where
  hashWithSalt s (Path hash name) = s `hashWithSalt` hash `hashWithSalt` name
