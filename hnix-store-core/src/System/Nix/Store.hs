{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.Store
  ( PathName, pathNameContents, pathName
  , PathHashAlgo, Path(..)
  , StoreEffects(..)
  , SubstitutablePathInfo(..)
  ) where

import Crypto.Hash (Digest)
import Crypto.Hash.Truncated (Truncated)
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteArray as B
import Data.Text (Text)
import Text.Regex.Base.RegexLike (makeRegex, matchTest)
import Text.Regex.TDFA.Text (Regex)
import Data.Hashable (Hashable(..), hashPtrWithSalt)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import System.IO.Unsafe (unsafeDupablePerformIO)

import System.Nix.Path
import System.Nix.Nar


-- | Interactions with the Nix store.
--
-- @rootedPath@: A path plus a witness to the fact that the path is
-- reachable from a root whose liftime is at least as long as the
-- @rootedPath@ reference itself, when the implementation supports
-- this.
--
-- @validPath@: A @rootedPath@ plus a witness to the fact that the
-- path is valid. On implementations that support temporary roots,
-- this implies that the path will remain valid so long as the
-- reference is held.
--
-- @m@: The monad the effects operate in.
data StoreEffects rootedPath validPath m =
  StoreEffects
    { -- | Project out the underlying 'Path' from a 'rootedPath'
      fromRootedPath :: !(rootedPath -> Path)
    , -- | Project out the underlying 'rootedPath' from a 'validPath'
      fromValidPath :: !(validPath -> rootedPath)
    , -- | Which of the given paths are valid?
      validPaths :: !(HashSet rootedPath -> m (HashSet validPath))
    , -- | Get the paths that refer to a given path.
      referrers :: !(validPath -> m (HashSet Path))
    , -- | Get a root to the 'Path'.
      rootedPath :: !(Path -> m rootedPath)
    , -- | Get information about substituters of a set of 'Path's
      substitutablePathInfos ::
        !(HashSet Path -> m (HashMap Path SubstitutablePathInfo))
    , -- | Get the currently valid derivers of a 'Path'.
      validDerivers :: !(Path -> m (HashSet Path))
    , -- | Get the outputs of the derivation at a 'Path'.
      derivationOutputs :: !(validPath -> m (HashSet Path))
    , -- | Get the output names of the derivation at a 'Path'.
      derivationOutputNames :: !(validPath -> m (HashSet Text))
    , -- | Get a full 'Path' corresponding to a given 'Digest'.
      pathFromHashPart :: !(Digest PathHashAlgo -> m Path)
    , -- | Add a non-nar file to the store
      addFile :: !(BS.ByteString -> m validPath)
    }
