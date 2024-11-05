{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Nix.Store.Remote.Types.StoreRequest
  ( StoreRequest(..)
  ) where

import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.HashSet (HashSet)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Some (Some(Some))

import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Derivation (BasicDerivation, DerivationOutput)
import System.Nix.DerivedPath (DerivedPath)
import System.Nix.Hash (HashAlgo)
import System.Nix.Signature (Signature)
import System.Nix.FileContentAddress (FileIngestionMethod)
import System.Nix.Store.Types (RepairMode)
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.Types.GC (GCOptions, GCResult, GCRoot)
import System.Nix.Store.Remote.Types.CheckMode (CheckMode)
import System.Nix.Store.Remote.Types.NoReply (NoReply)
import System.Nix.Store.Remote.Types.Query.Missing (Missing)
import System.Nix.Store.Remote.Types.StoreText (StoreText)
import System.Nix.Store.Remote.Types.SubstituteMode (SubstituteMode)
import System.Nix.Store.Remote.Types.SuccessCodeReply (SuccessCodeReply)

data StoreRequest :: Type -> Type where
  -- | Add @NarSource@ to the store.
  AddToStore
    :: StorePathName -- ^ Name part of the newly created @StorePath@
    -> FileIngestionMethod -- ^ Add target directory recursively
    -> Some HashAlgo -- ^ Nar hashing algorithm
    -> RepairMode -- ^ Only used by local store backend
    -> StoreRequest StorePath

  -- | Add a NAR with Metadata to the store.
  AddToStoreNar
    :: StorePath
    -> Metadata StorePath
    -> RepairMode
    -> CheckMode -- ^ Whether to check signatures
    -> StoreRequest NoReply

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
    -> Set Signature
    -> StoreRequest SuccessCodeReply

  AddIndirectRoot
    :: StorePath
    -> StoreRequest SuccessCodeReply

  -- | Add temporary garbage collector root.
  --
  -- This root is removed as soon as the client exits.
  AddTempRoot
    :: StorePath
    -> StoreRequest SuccessCodeReply

  -- | Build paths if they are an actual derivations.
  --
  -- If derivation output paths are already valid, do nothing.
  BuildPaths
    :: Set DerivedPath
    -> BuildMode
    -> StoreRequest SuccessCodeReply

  BuildDerivation
    :: StorePath
    -> BasicDerivation
    -> BuildMode
    -> StoreRequest BuildResult

  CollectGarbage
    :: GCOptions
    -> StoreRequest GCResult

  EnsurePath
    :: StorePath
    -> StoreRequest SuccessCodeReply

  -- | Find garbage collector roots.
  FindRoots
    :: StoreRequest (Map GCRoot StorePath)

  IsValidPath
    :: StorePath
    -> StoreRequest Bool

  -- | Fetch a NAR from the server
  NarFromPath
    :: StorePath
    -> StoreRequest NoReply

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
    -> StoreRequest Missing

  OptimiseStore
    :: StoreRequest SuccessCodeReply

  SyncWithGC
    :: StoreRequest SuccessCodeReply

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
  Some (AddToStoreNar a b c d) == Some (AddToStoreNar a' b' c' d') = (a, b, c, d) == (a', b', c', d')
  Some (AddTextToStore a b c) == Some (AddTextToStore a' b' c') = (a, b, c) == (a', b', c')
  Some (AddSignatures a b) == Some (AddSignatures a' b') = (a, b) == (a', b')
  Some (AddIndirectRoot a) == Some (AddIndirectRoot a') = a == a'
  Some (AddTempRoot a) == Some (AddTempRoot a') = a == a'
  Some (BuildPaths a b) == Some (BuildPaths a' b') = (a, b) == (a', b')
  Some (BuildDerivation a b c) == Some (BuildDerivation a' b' c') = (a, b, c) == (a', b', c')
  Some (CollectGarbage a) == Some (CollectGarbage a') = a == a'
  Some (EnsurePath a) == Some (EnsurePath a') = a == a'
  Some (FindRoots) == Some (FindRoots) = True
  Some (IsValidPath a) == Some (IsValidPath a') = a == a'
  Some (NarFromPath a) == Some (NarFromPath a') = a == a'
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
