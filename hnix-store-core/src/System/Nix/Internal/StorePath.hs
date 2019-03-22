{-|
Description : Representation of Nix store paths.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module System.Nix.Internal.StorePath where
import System.Nix.Hash (HashAlgorithm(Truncated, SHA256), Digest, encodeBase32)
import Text.Regex.Base.RegexLike (makeRegex, matchTest)
import Text.Regex.TDFA.Text (Regex)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Hashable (Hashable(..))
import Data.HashSet (HashSet)
import Data.Proxy (Proxy(..))

-- | A path in a Nix store.
--
-- From the Nix thesis: A store path is the full path of a store
-- object. It has the following anatomy: storeDir/hashPart-name.
--
-- @storeDir@: The root of the Nix store (e.g. \/nix\/store).
--
-- See the 'StoreDir' haddocks for details on why we represent this at
-- the type level.
data StorePath (storeDir :: StoreDir) = StorePath
  { -- | The 160-bit hash digest reflecting the "address" of the name.
    -- Currently, this is a truncated SHA256 hash.
    storePathHash :: !(Digest StorePathHashAlgo)
  , -- | The (typically human readable) name of the path. For packages
    -- this is typically the package name and version (e.g.
    -- hello-1.2.3).
    storePathName :: !StorePathName
  }

instance Hashable (StorePath storeDir) where
  hashWithSalt s (StorePath {..}) =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character (i.e. it must match
-- 'storePathNameRegex').
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Hashable)

-- | The hash algorithm used for store path hashes.
type StorePathHashAlgo = 'Truncated 20 'SHA256

-- | A set of 'StorePath's.
type StorePathSet storeDir = HashSet (StorePath storeDir)

-- | A type-level representation of the root directory of a Nix store.
--
-- The extra complexity of type indices requires justification.
-- Fundamentally, this boils down to the fact that there is little
-- meaningful sense in which 'StorePath's rooted at different
-- directories are of the same type, i.e. there are few if any
-- non-trivial non-contrived functions or data types that could
-- equally well accept 'StorePath's from different stores. In current
-- practice, any real application dealing with Nix stores (including,
-- in particular, the Nix expression language) only operates over one
-- store root and only cares about 'StorePath's belonging to that
-- root. One could imagine a use case that cares about multiple store
-- roots at once (e.g. the normal \/nix\/store along with some private
-- store at \/root\/nix\/store to contain secrets), but in that case
-- distinguishing 'StorePath's that belong to one store or the other
-- is even /more/ critical: Most operations will only be correct over
-- one of the stores or another, and it would be an error to mix and
-- match (e.g. a 'StorePath' in one store could not legitimately refer
-- to one in another).
--
-- As of @5886bc5996537fbf00d1fcfbb29595b8ccc9743e@, the C++ Nix
-- codebase contains 30 separate places where we assert that a given
-- store dir is, in fact, in the store we care about; those run-time
-- assertions could be completely removed if we had stronger types
-- there. Moreover, there are dozens of other cases where input coming
-- from the user, from serializations, etc. is parsed and then
-- required to be in the appropriate store; this case is the
-- equivalent of an existentially quantified version of 'StorePath'
-- and, notably, requiring at runtime that the index matches the
-- ambient store directory we're working in. In every case where a
-- path is treated as a store path, there is exactly one legitimate
-- candidate for the store directory it belongs to.
--
-- It may be instructive to consider the example of "chroot stores".
-- Since Nix 2.0, it has been possible to have a store actually live
-- at one directory (say, $HOME\/nix\/store) with a different
-- effective store directory (say, \/nix\/store). Nix can build into
-- a chroot store by running the builds in a mount namespace where the
-- store is at the effective store directory, can download from a
-- binary cache containing paths for the effective store directory,
-- and can run programs in the store that expect to be living at the
-- effective store directory (via nix run). When viewed as store paths
-- (rather than random files in the filesystem), paths in a chroot
-- store have nothing in common with paths in a non-chroot store that
-- lives in the same directory, and a lot in common with paths in a
-- non-chroot store that lives in the effective store directory of the
-- store in question. Store paths in stores with the same effective
-- store directory share the same hashing scheme, can be copied
-- between each other, etc. Store paths in stores with different
-- effective store directories have no relationship to each other that
-- they don't have to arbitrary other files.
type StoreDir = Symbol

-- | Smart constructor for 'StorePathName' that ensures the underlying
-- content invariant is met.
makeStorePathName :: Text -> Maybe StorePathName
makeStorePathName n = case matchTest storePathNameRegex n of
  True  -> Just $ StorePathName n
  False -> Nothing

-- | Regular expression to match valid store path names.
storePathNameRegex :: Regex
storePathNameRegex = makeRegex r
  where
    r :: String
    r = "[a-zA-Z0-9\\+\\-\\_\\?\\=][a-zA-Z0-9\\+\\-\\.\\_\\?\\=]*"

-- | Copied from @RawFilePath@ in the @unix@ package, duplicated here
-- to avoid the dependency.
type RawFilePath = ByteString

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath
  :: forall storeDir . (KnownStoreDir storeDir)
  => StorePath storeDir
  -> RawFilePath
storePathToRawFilePath (StorePath {..}) = BS.concat
    [ root
    , "/"
    , hashPart
    , "-"
    , name
    ]
  where
    root = storeDirVal @storeDir
    hashPart = encodeUtf8 $ encodeBase32 storePathHash
    name = encodeUtf8 $ unStorePathName storePathName

-- | Get a value-level representation of a 'KnownStoreDir'
storeDirVal :: forall storeDir . (KnownStoreDir storeDir)
            => ByteString
storeDirVal = BC.pack $ symbolVal @storeDir Proxy

-- | A 'StoreDir' whose value is known at compile time.
type KnownStoreDir = KnownSymbol
