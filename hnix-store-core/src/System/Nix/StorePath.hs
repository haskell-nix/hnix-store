{-|
Description : Creating and manipulating Nix store paths.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}
module System.Nix.StorePath
  ( StorePathHashAlgo
  , StorePathName
  , storePathNameText
  , storePathName
  , StorePath(..)
  , StorePathSet
  , StorePathInfo(..)
  , ContentAddressedHash(..)
  , HashMode(..)
  ) where

import Data.Word (Word64)
import GHC.TypeLits (Symbol)
import System.Nix.Hash
import Data.Time (UTCTime)
import Data.Hashable (Hashable (..))
import Data.HashSet (HashSet)
import Data.Text (Text)
import Text.Regex.Base.RegexLike (makeRegex, matchTest)
import Text.Regex.TDFA.Text (Regex)

-- | The hash algorithm used for store path hashes.
type StorePathHashAlgo = 'Truncated 20 'SHA256

-- | The name portion of a Nix path.
--
-- Must be composed of a-z, A-Z, 0-9, +, -, ., _, ?, and =, can't
-- start with a ., and must have at least one character.
newtype StorePathName = StorePathName
  { storePathNameText :: Text
    -- ^ The contents of the path name
  } deriving (Eq, Ord, Show, Hashable)

-- | A regular expression for matching a valid 'StorePathName'
nameRegex :: Regex
nameRegex =
  makeRegex "[a-zA-Z0-9\\+\\-\\_\\?\\=][a-zA-Z0-9\\+\\-\\.\\_\\?\\=]*"

-- | Construct a 'StorePathName', checking that the provided contents are valid.
storePathName :: Text -> Maybe StorePathName
storePathName n = case matchTest nameRegex n of
  True  -> Just $ StorePathName n
  False -> Nothing

-- | A path in a store.
--
-- @root@: The root path of the store (e.g. "/nix/store").
data StorePath (root :: Symbol) = StorePath
  { pathHash :: !(Digest StorePathHashAlgo)
  , pathName :: !StorePathName
  } deriving (Eq, Ord, Show)

type StorePathSet root = HashSet (StorePath root)

-- | Metadata about a valid @Path@ in the store.
data StorePathInfo store = StorePathInfo
  { -- |The path itself
    path             :: !(StorePath store)
  , -- | The .drv which led to this 'Path', if any/known.
    deriver          :: !(Maybe (StorePath store))
  , -- | The hash of the serialization of this path.
    narHash          :: !NamedDigest
  , -- | The references of the 'Path'.
    references       :: !(StorePathSet store)
  , -- | When this store path was registered valid.
    registrationTime :: !UTCTime
  , -- | The size of the uncompressed NAR serialization of this
    -- 'StorePath'.
    narSize          :: !Word64
  , -- | Whether the path is ultimately trusted, that is, it's a
    -- derivation output that was built locally.
    ultimate         :: !Bool
  , -- | Signatures attesting to the validity of this registration.
    sigs             :: ![Text] -- TODO better type?
  , -- | Whether or not the store path is content-addressed, and if so
    -- what is the hash that gives its address?
    ca               :: !(Maybe ContentAddressedHash)
  }

-- | The different types of content-addressed hashing we have in Nix.
data ContentAddressedHash
  = RegularFile (Digest 'SHA256)
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

instance Hashable (StorePath store) where
  hashWithSalt s (StorePath {..}) =
    s `hashWithSalt` pathHash `hashWithSalt` pathName
