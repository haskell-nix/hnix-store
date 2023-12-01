{-# language GADTs #-}
{-# language Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Nix.Store.Remote.GADT
  ( StoreRequest(..)
  ) where

import Data.ByteString (ByteString)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.HashSet (HashSet)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Some (Some(Some))

import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Derivation (Derivation)
import System.Nix.DerivedPath (DerivedPath)
import System.Nix.Hash (HashAlgo)
import System.Nix.Store.Types (RepairMode)
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.Types.CheckMode (CheckMode)
import System.Nix.Store.Remote.Types.StoreText (StoreText)
import System.Nix.Store.Remote.Types.SubstituteMode (SubstituteMode)

data StoreRequest :: Type -> Type where
  -- | Add @NarSource@ to the store.
  AddToStore
    :: StorePathName -- ^ Name part of the newly created @StorePath@
    -> Bool -- ^ Add target directory recursively
    -> Some HashAlgo -- ^ Nar hashing algorithm
--  -> (forall m . MonadIO m => NarSource m) -- ^ provide nar stream
-- Not part of StoreRequest
-- as it would require StoreRequest (m :: Type -> Type) :: Type -> Type
-- for which we cannot derive anything
--
-- Also the thing is the only special case
-- and it is always sent *after* the other
-- information so it can be handled
-- separately after that. Hopefully.
    -> RepairMode -- ^ Only used by local store backend
    -> StoreRequest StorePath

  -- | Add text to store.
  --
  -- Reference accepts repair but only uses it
  -- to throw error in case of remote talking to nix-daemon.
  AddTextToStore
    :: StoreText
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

deriving instance Eq (StoreRequest a)
deriving instance Show (StoreRequest a)

deriveGEq ''StoreRequest
deriveGCompare ''StoreRequest
deriveGShow ''StoreRequest

instance {-# OVERLAPPING #-} Eq (Some StoreRequest) where
  Some (AddToStore a b c d) == Some (AddToStore a' b' c' d') = (a, b, c, d) == (a', b', c', d')
  Some (AddTextToStore a b c) == Some (AddTextToStore a' b' c') = (a, b, c) == (a', b', c')
  Some (AddSignatures a b) == Some (AddSignatures a' b') = (a, b) == (a', b')
  Some (AddIndirectRoot a) == Some (AddIndirectRoot a') = a == a'
  Some (AddTempRoot a) == Some (AddTempRoot a') = a == a'
  Some (BuildPaths a b) == Some (BuildPaths a' b') = (a, b) == (a', b')
  Some (BuildDerivation a b c) == Some (BuildDerivation a' b' c') = (a, b, c) == (a', b', c')
  Some (EnsurePath a) == Some (EnsurePath a') = a == a'
  Some (FindRoots) == Some (FindRoots) = True
  Some (IsValidPath a) == Some (IsValidPath a') = a == a'
  Some (QueryValidPaths a b) == Some (QueryValidPaths a' b') = (a, b) == (a', b')
  Some QueryAllValidPaths == Some QueryAllValidPaths = True
  Some (QuerySubstitutablePaths a) == Some (QuerySubstitutablePaths a') = a == a'
  Some (QueryPathInfo a) == Some (QueryPathInfo a') = a == a'
  Some (QueryReferrers a) == Some (QueryReferrers a') = a == a'
  Some (QueryValidDerivers a) == Some (QueryValidDerivers a') = a == a'
  Some (QueryDerivationOutputs a) == Some (QueryDerivationOutputs a') = a == a'
  Some (QueryDerivationOutputNames a) == Some (QueryDerivationOutputNames a') = a == a'
  Some (QueryPathFromHashPart a) == Some (QueryPathFromHashPart a') = a == a'
  Some (QueryMissing a) == Some (QueryMissing a') = a == a'
  Some OptimiseStore == Some OptimiseStore = True
  Some SyncWithGC == Some SyncWithGC = True
  Some (VerifyStore a b) == Some (VerifyStore a' b') = (a, b) == (a', b')

  _ == _ = False
