{-|
Description : Effects for interacting with the Nix store.
-}
{-# LANGUAGE RankNTypes #-}
module System.Nix.Store where

import Data.ByteString.Lazy (ByteString)
import System.Nix.Hash (NamedAlgorithm)
import System.Nix.StorePath
import System.Nix.Nar (Nar)

-- | Effect for interactions with the Nix store.
--
-- @root@: The root path of the store (e.g. "/nix/store").
--
-- @m@: The monad the effects operate in.
--
-- A valid instance should follow the appropriate hashing algorithms
-- for effects which add a new path to the store.
data StoreEffects root m = StoreEffects
  { -- | Add a regular file to the store with the given references,
    -- hashed with 'SHA256'.
    regularFileToStore
      :: StorePathName
      -> ByteString
      -> StorePathSet root
      -> m (StorePath root)
  , -- ^ Add a fixed file (possibly not regular) to the store with a
    -- given hash algorithm.
    --
    -- Note that conceptually this functionality overlaps with
    -- 'regularFileToStore' (when the 'HashMode' is 'Flat @SHA256' and
    -- the references set is empty), but for legacy reasons these
    -- follow a different underlying algorithm for getting the store
    -- path.
    --
    -- If the 'HashMode' is 'Fixed', the top level FSO of the 'Nar'
    -- must be a 'Regular' object.
    fixedFileToStore
      :: forall a . (NamedAlgorithm a)
      => StorePathName
      -> HashMode a
      -> Nar
      -> m (StorePath root)
  , -- ^ Import a serialization of a valid path into the store.
    --
    -- 'CheckSigs' is ignored if not a trusted user.
    importPath
      :: StorePathInfo root
      -> Nar
      -> Repair
      -> CheckSigs
      -> m ()
  }


-- | Flag to indicate whether a command should overwrite a specified
-- path if it already exists (in an attempt to fix issues).
data Repair = Repair | DontRepair

-- | Flag to indicate whether signatures should be validated on an
-- imported archive.
data CheckSigs = CheckSigs | DontCheckSigs
