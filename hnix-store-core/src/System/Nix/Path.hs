{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.Path
  ( FilePathPart(..)
  , PathHashAlgo
  , Path(..)
  , PathSet
  , SubstitutablePathInfo(..)
  , ValidPathInfo(..)
  , PathName(..)
  , filePathPart
  , pathName
  , Roots
  ) where

import           System.Nix.Hash           (Digest(..),
                                            HashAlgorithm'(Truncated, SHA256))
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
type PathHashAlgo = Truncated 20 SHA256


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
data Path = Path !(Digest PathHashAlgo) !PathName
  deriving (Eq, Ord, Show)

type PathSet = HashSet Path

-- | Information about substitutes for a 'Path'.
data SubstitutablePathInfo = SubstitutablePathInfo
  { -- | The .drv which led to this 'Path'.
    deriver      :: !(Maybe Path)
  , -- | The references of the 'Path'
    references   :: !PathSet
  , -- | The (likely compressed) size of the download of this 'Path'.
    downloadSize :: !Integer
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSize      :: !Integer
  } deriving (Eq, Ord, Show)

-- | Information about @Path@
data ValidPathInfo = ValidPathInfo
  { -- | Path itself
    path             :: !Path
  , -- | The .drv which led to this 'Path'.
    deriverVP        :: !(Maybe Path)
  , -- | NAR hash
    narHash          :: !Text
  , -- | The references of the 'Path'
    referencesVP     :: !PathSet
  , -- | Registration time should be time_t
    registrationTime :: !Integer
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSizeVP        :: !Integer
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
  } deriving (Eq, Ord, Show)

-- | A valid filename or directory name
newtype FilePathPart = FilePathPart { unFilePathPart :: BSC.ByteString }
  deriving (Eq, Ord, Show)

-- | Construct FilePathPart from Text by checking that there
--   are no '/' or '\\NUL' characters
filePathPart :: BSC.ByteString -> Maybe FilePathPart
filePathPart p = case BSC.any (`elem` ['/', '\NUL']) p of
  False -> Just $ FilePathPart p
  True  -> Nothing

type Roots = Map Path Path

instance Hashable Path where
  hashWithSalt s (Path hash name) = s `hashWithSalt` hash `hashWithSalt` name
