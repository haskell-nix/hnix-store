{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.Nix.Path
  ( FilePathPart(..)
  , filePathPart
  , HashMode(..)
  , PathInfo(..)
  , Path(..)
  , PathHashAlgo
  , PathName(..)
  , PathSet
  , pathName
  , pathToText
  ) where

import Data.Word
import           GHC.TypeLits
import           Data.Time
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.Hashable             (Hashable (..), hashPtrWithSalt)
import           Data.HashMap.Strict       (HashMap)
import           Data.HashSet              (HashSet)
import           Data.Map.Strict           (Map)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           Text.Regex.Base.RegexLike (makeRegex, matchTest)
import           Text.Regex.TDFA.Text      (Regex)

import           System.Nix.Hash
import           System.Nix.Hash           (Digest(..),
                                            HashAlgorithm(SHA256),
                                            HashForm'(Truncated),
                                            NamedAlgorithm)
import           System.Nix.Internal.Hash

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
  makeRegex ("[a-zA-Z0-9\\+\\-\\_\\?\\=][a-zA-Z0-9\\+\\-\\.\\_\\?\\=]*" :: String)

-- | Construct a 'PathName', assuming the provided contents are valid.
pathName :: Text -> Maybe PathName
pathName n = case matchTest nameRegex n of
  True  -> Just $ PathName n
  False -> Nothing

-- | A path in a store.
-- Does not include the path *to* the store, e.g. "/nix/store".
data Path = Path !(Digest PathHashAlgo) !PathName
  deriving (Eq, Ord, Show)

pathToText :: Text -> Path -> Text
pathToText storeDir (Path h nm) = storeDir <> "/" <> printAsBase32 h <> "-" <> pathNameContents nm

type PathSet = HashSet Path

-- | Metadata about a valid @Path@ in the store.
data PathInfo = PathInfo
  { -- | Path itself
    path             :: !Path
  , -- | The .drv which led to this 'Path'.
    deriver        :: !(Maybe Path)
  , -- | The hash of the serialization of this path.
    narHash          :: !AnyDigest
  , -- | The references of the 'Path'.
    references     :: !PathSet
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
  } --deriving (Eq, Ord, Show)

-- | The different types of content-addressed hashing we have in Nix.
data ContentAddressedHash
  = RegularFile (Digest ('Plain 'SHA256))
    -- ^ A regular file hashed like sha256sum.
  | forall algo . NamedAlgorithm algo =>
      FixedFile HashMode (Digest (Plain algo))
      -- ^ A file hashed via the add-fixed-file-to-store approach.
      -- This can in fact overlap with RegularFile (if the 'HashMode'
      -- is 'Flat @SHA256'), but the resulting Nix store hash is
      -- different for stupid legacy reasons.

-- | A specification of how to hash a file.
data HashMode
  = Flat -- ^ Normal hashing of a regular file.
  | Recursive -- ^ Hashing of a serialization of a file, compatible
              -- with directories and executable files as well as
              -- regular files.

instance Hashable Path where
  hashWithSalt s (Path hash name) = s `hashWithSalt` hash `hashWithSalt` name

-- | A valid filename or directory name
newtype FilePathPart = FilePathPart { unFilePathPart :: BSC.ByteString }
  deriving (Eq, Ord, Show)

-- | Construct FilePathPart from Text by checking that there
--   are no '/' or '\\NUL' characters
filePathPart :: BSC.ByteString -> Maybe FilePathPart
filePathPart p = case BSC.any (`elem` ['/', '\NUL']) p of
  False -> Just $ FilePathPart p
  True  -> Nothing
