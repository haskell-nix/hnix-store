{-# language GADTs #-}
{-# language Rank2Types #-}

module System.Nix.Store.Remote.GADT
  ( StoreRequest(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Some (Some)

import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Derivation (Derivation)
import System.Nix.DerivedPath (DerivedPath)
import System.Nix.Hash (HashAlgo)
import System.Nix.Nar (NarSource)
import System.Nix.Store.Types (RepairMode)
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.Types.CheckMode (CheckMode)
import System.Nix.Store.Remote.Types.SubstituteMode (SubstituteMode)

data StoreRequest :: Type -> Type where
  -- | Add @NarSource@ to the store.
  AddToStore
    :: StorePathName -- ^ Name part of the newly created @StorePath@
    -> Bool -- ^ Add target directory recursively
    -> Some HashAlgo
    -> (forall m . MonadIO m => NarSource m) -- ^ provide nar stream
    -> RepairMode -- ^ Only used by local store backend
    -> StoreRequest StorePath

  -- | Add text to store.
  --
  -- Reference accepts repair but only uses it
  -- to throw error in case of remote talking to nix-daemon.
  AddTextToStore
    :: Text -- ^ Name of the text
    -> Text -- ^ Actual text to add
    -> HashSet StorePath -- ^ Set of @StorePath@s that the added text references
    -> RepairMode -- ^ Repair mode, must be @RepairMode_DontRepair@ in case of remote backend
    -> StoreRequest StorePath

  AddSignatures
    :: StorePath
    -> [ByteString]
    -> StoreRequest ()

  -- | Add temporary garbage collector root.
  --
  -- This root is removed as soon as the client exits.
  AddIndirectRoot
    :: StorePath
    -> StoreRequest ()

  AddTempRoot
    :: StorePath
    -> StoreRequest ()

  -- | Build paths if they are an actual derivations.
  --
  -- If derivation output paths are already valid, do nothing.
  BuildPaths
    :: Set DerivedPath
    -> BuildMode
    -> StoreRequest ()

  BuildDerivation
    :: StorePath
    -> Derivation StorePath Text
    -> BuildMode
    -> StoreRequest BuildResult

  EnsurePath
    :: StorePath
    -> StoreRequest ()

  -- | Find garbage collector roots.
  FindRoots
    :: StoreRequest (Map ByteString StorePath)

  IsValidPath
    :: StorePath
    -> StoreRequest Bool

  -- | Query valid paths from set, optionally try to use substitutes.
  QueryValidPaths
    :: HashSet StorePath
    -- ^ Set of @StorePath@s to query
    -> SubstituteMode
    -- ^ Try substituting missing paths when @SubstituteMode_DoSubstitute@
    -> StoreRequest (HashSet StorePath)

  QueryAllValidPaths
    :: StoreRequest (HashSet StorePath)

  QuerySubstitutablePaths
    :: HashSet StorePath
    -> StoreRequest (HashSet StorePath)

  QueryPathInfo
    :: StorePath
    -> StoreRequest (Maybe (Metadata StorePath))

  QueryReferrers
    :: StorePath
    -> StoreRequest (HashSet StorePath)

  QueryValidDerivers
    :: StorePath
    -> StoreRequest (HashSet StorePath)

  QueryDerivationOutputs
    :: StorePath
    -> StoreRequest (HashSet StorePath)

  QueryDerivationOutputNames
    :: StorePath
    -> StoreRequest (HashSet StorePathName)

  QueryPathFromHashPart
    :: StorePathHashPart
    -> StoreRequest StorePath

  QueryMissing
    :: Set DerivedPath
    -> StoreRequest
      ( HashSet StorePath -- Paths that will be built
      , HashSet StorePath -- Paths that have substitutes
      , HashSet StorePath -- Unknown paths
      , Integer -- Download size
      , Integer -- Nar size?
      )

  OptimiseStore
    :: StoreRequest ()

  SyncWithGC
    :: StoreRequest ()

  -- returns True on errors
  VerifyStore
    :: CheckMode
    -> RepairMode
    -> StoreRequest Bool
