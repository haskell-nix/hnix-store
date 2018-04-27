{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.Store
  ( PathName, pathNameContents, pathName
  , PathHashAlgo, Path(..)
  , ReadonlyStoreEffects(..)
  ) where

import Crypto.Hash (Digest)
import Crypto.Hash.Truncated (Truncated)
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteArray as B
import Data.Text (Text)
import Text.Regex.Base.RegexLike (makeRegex, matchTest)
import Text.Regex.TDFA.Text (Regex)
import Data.Hashable (Hashable(..), hashPtrWithSalt)
import Data.HashSet (HashSet)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | The name portion of a Nix path.
--
-- Must be composed of a-z, A-Z, 0-9, +, -, ., _, ?, and =, can't
-- start with a ., and must have at least one character.
newtype PathName = PathName
  { pathNameContents :: Text -- ^ The contents of the path name
  } deriving (Hashable)

-- | A regular expression for matching a valid 'PathName'
nameRegex :: Regex
nameRegex =
  makeRegex "[a-zA-Z0-9\\+\\-\\_\\?\\=][a-zA-Z0-9\\+\\-\\.\\_\\?\\=]*"

-- | Construct a 'PathName', assuming the provided contents are valid.
pathName :: Text -> Maybe PathName
pathName n = case matchTest nameRegex n of
  True -> Just $ PathName n
  False -> Nothing

-- | The hash algorithm used for store path hashes.
type PathHashAlgo = Truncated SHA256 20

-- | A path in a store.
data Path = Path !(Digest PathHashAlgo) !PathName

-- | Wrapper to defined a 'Hashable' instance for 'Digest'.
newtype HashableDigest a = HashableDigest (Digest a)

instance Hashable (HashableDigest a) where
  hashWithSalt s (HashableDigest d) = unsafeDupablePerformIO $
    B.withByteArray d $ \ptr -> hashPtrWithSalt ptr (B.length d) s

instance Hashable Path where
  hashWithSalt s (Path digest name) =
    s `hashWithSalt`
    (HashableDigest digest) `hashWithSalt` name

-- | Read-only interactions with a store.
--
-- 'rootedPath': A path plus a witness to the fact that the path is
-- reachable from a root whose liftime is at least as long as the
-- 'rootedPath' reference itself, when the implementation supports
-- this.
--
-- 'validPath': A 'rootedPath' plus a witness to the fact that the
-- path is valid. On implementations that support temporary roots,
-- this implies that the path will remain valid so long as the
-- reference is held.
--
-- 'm': The monad the effects operate in.
data ReadonlyStoreEffects rootedPath validPath m =
  ReadonlyStoreEffects
    { -- | Project out the underlying 'Path' from a 'rootedPath'
      fromRootedPath :: !(rootedPath -> Path)
    , -- | Project out the underlying 'rootedPath' from a 'validPath'
      fromValidPath :: !(validPath -> rootedPath)
    , -- | Is the given path valid?
      validPath :: !(rootedPath -> m (Maybe validPath))
    , -- | Get the paths that refer to a given path.
      referrers :: !(validPath -> m (HashSet Path))
    }
