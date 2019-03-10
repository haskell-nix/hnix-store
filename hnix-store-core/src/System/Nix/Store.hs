{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module System.Nix.Store where

import Data.ByteString.Lazy (ByteString)
import System.Nix.Hash (NamedAlgorithm, HashAlgorithm)
import System.Nix.Path
import System.Nix.Nar

-- | Interactions with the Nix store.
--
-- @root@: The root path of the store (e.g. "/nix/store").
--
-- @m@: The monad the effects operate in.
data StoreEffects root m = StoreEffects
  { regularFileToStore -- ^ Add a regular file to the store with the
                       -- given references, hashed with 'SHA256'.
      :: PathName -- ^ The name of the path.
      -> ByteString -- ^ The contents of the file.
      -> PathSet root -- ^ The references of the path.
      -> m (Path root) -- ^ The added store path.
  , fixedFileToStore -- ^ Add a fixed file (possibly not regular) to
                     -- the store with the diven hash algorithm.
      :: forall a . (NamedAlgorithm a)
      => PathName -- ^ The name of the path.
      -> HashMode a -- ^ How to hash the file.
      -> Nar -- ^ A nix archive dump of the file.
      -> m (Path root)
  , importPath -- ^ Import a serialization of a valid path into the
               -- store.
      :: PathInfo root -- ^ Store path metadata.
      -> Nar -- ^ A nix archive dump of file.
      -> Repair -- ^ Whether to overwrite the path if it is already
                -- valid in the store.
      -> CheckSigs -- ^ Whether to validate the signatures on the
                   -- archive. Ignored if not a trusted user.
      -> m ()
  }


-- | Flag to indicate whether a command should overwrite a specified
-- path if it already exists (in an attempt to fix issues).
data Repair = Repair | DontRepair

-- | Flag to indicate whether signatures should be validated on an
-- imported archive.
data CheckSigs = CheckSigs | DontCheckSigs
