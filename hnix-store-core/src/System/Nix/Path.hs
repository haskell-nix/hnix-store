{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.Path
  ( PathHashAlgo
  , Path(..)
  , SubstitutablePathInfo(..)
  , PathName(..)
  , pathName
  ) where

import Crypto.Hash               (Digest)
import Crypto.Hash.Truncated     (Truncated)
import Crypto.Hash.Algorithms    (SHA256)
import qualified Data.ByteArray  as B
import Data.Text                 (Text)
import Text.Regex.Base.RegexLike (makeRegex, matchTest)
import Text.Regex.TDFA.Text      (Regex)
import Data.Hashable             (Hashable(..), hashPtrWithSalt)
import Data.HashSet              (HashSet)
import Data.HashMap.Strict       (HashMap)
import System.IO.Unsafe          (unsafeDupablePerformIO)

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


-- | Information about substitutes for a 'Path'.
data SubstitutablePathInfo = SubstitutablePathInfo
  { -- | The .drv which led to this 'Path'.
    deriver :: !(Maybe Path)
  , -- | The references of the 'Path'
    references :: !(HashSet Path)
  , -- | The (likely compressed) size of the download of this 'Path'.
    downloadSize :: !Integer
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSize :: !Integer
  }
