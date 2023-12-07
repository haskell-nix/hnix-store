module System.Nix.Store.Remote.Client
  ( addToStore
  , addTextToStore
  , addSignatures
  , addTempRoot
  , addIndirectRoot
  , buildPaths
  , buildDerivation
  , collectGarbage
  , ensurePath
  , findRoots
  , isValidPath
  , queryValidPaths
  , queryAllValidPaths
  , querySubstitutablePaths
  , queryPathInfo
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , queryMissing
  , optimiseStore
  , syncWithGC
  , verifyStore
  , module System.Nix.Store.Remote.Client.Core
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Some (Some)
import Data.Text (Text)

import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Derivation (Derivation)
import System.Nix.DerivedPath (DerivedPath)
import System.Nix.Hash (HashAlgo(..))
import System.Nix.Nar (NarSource)
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath, StorePathHashPart, StorePathName)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Types.GC (GCOptions, GCResult, GCRoot)
import System.Nix.Store.Remote.Types.CheckMode (CheckMode)
import System.Nix.Store.Remote.Types.Query.Missing (Missing)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.StoreText (StoreText)
import System.Nix.Store.Remote.Types.SubstituteMode (SubstituteMode)
import System.Nix.Store.Remote.Client.Core
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))

-- | Add `NarSource` to the store
addToStore
  :: MonadRemoteStore m
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource IO         -- ^ Provide nar stream
  -> FileIngestionMethod  -- ^ Add target directory recursively
  -> Some HashAlgo        -- ^
  -> RepairMode           -- ^ Only used by local store backend
  -> m StorePath
addToStore name source method hashAlgo repair = do
  Control.Monad.when
    (repair == RepairMode_DoRepair)
    $ throwError RemoteStoreError_RapairNotSupportedByRemoteStore

  setNarSource source
  doReq (AddToStore name method hashAlgo repair)

-- | Add @StoreText@ to the store
-- Reference accepts repair but only uses it
-- to throw error in case of remote talking to nix-daemon.
addTextToStore
  :: MonadRemoteStore m
  => StoreText
  -> HashSet StorePath -- ^ Set of `StorePath`s that the added text references
  -> RepairMode        -- ^ Repair mode, must be `RepairMode_DontRepair` for remote backend
                       --   (only valid for local store)
  -> m StorePath
addTextToStore stext references repair = do
  Control.Monad.when
    (repair == RepairMode_DoRepair)
    $ throwError RemoteStoreError_RapairNotSupportedByRemoteStore

  doReq (AddTextToStore stext references repair)

-- | Add @Signature@s to a store path
addSignatures
  :: MonadRemoteStore m
  => StorePath
  -> Set Signature
  -> m ()
addSignatures p signatures = doReq (AddSignatures p signatures)

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot
  :: MonadRemoteStore m
  => StorePath
  -> m ()
addTempRoot = doReq . AddTempRoot

-- | Add indirect garbage collector root.
addIndirectRoot
  :: MonadRemoteStore m
  => StorePath
  -> m ()
addIndirectRoot = doReq . AddIndirectRoot

-- | Build a derivation available at @StorePath@
buildDerivation
  :: MonadRemoteStore m
  => StorePath
  -> Derivation StorePath Text
  -> BuildMode
  -> m BuildResult
buildDerivation a b c = doReq (BuildDerivation a b c)

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths
  :: MonadRemoteStore m
  => Set DerivedPath
  -> BuildMode
  -> m ()
buildPaths a b = doReq (BuildPaths a b)

collectGarbage
  :: MonadRemoteStore m
  => GCOptions
  -> m GCResult
collectGarbage = doReq . CollectGarbage

ensurePath
  :: MonadRemoteStore m
  => StorePath
  -> m ()
ensurePath = doReq . EnsurePath

-- | Find garbage collector roots.
findRoots
  :: MonadRemoteStore m
  => m (Map GCRoot StorePath)
findRoots = doReq FindRoots

isValidPath
  :: MonadRemoteStore m
  => StorePath
  -> m Bool
isValidPath = doReq . IsValidPath

-- | Query valid paths from a set,
-- optionally try to use substitutes
queryValidPaths
  :: MonadRemoteStore m
  => HashSet StorePath
  -- ^ Set of @StorePath@s to query
  -> SubstituteMode
  -- ^ Try substituting missing paths when @SubstituteMode_DoSubstitute@
  -> m (HashSet StorePath)
queryValidPaths a b = doReq (QueryValidPaths a b)

-- | Query all valid paths
queryAllValidPaths
  :: MonadRemoteStore m
  => m (HashSet StorePath)
queryAllValidPaths = doReq QueryAllValidPaths

-- | Query a set of paths substituable from caches
querySubstitutablePaths
  :: MonadRemoteStore m
  => HashSet StorePath
  -> m (HashSet StorePath)
querySubstitutablePaths = doReq . QuerySubstitutablePaths

-- | Query path metadata
queryPathInfo
  :: MonadRemoteStore m
  => StorePath
  -> m (Maybe (Metadata StorePath))
queryPathInfo = doReq . QueryPathInfo

queryReferrers
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePath)
queryReferrers = doReq . QueryReferrers

queryValidDerivers
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePath)
queryValidDerivers = doReq . QueryValidDerivers

queryDerivationOutputs
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePath)
queryDerivationOutputs = doReq . QueryDerivationOutputs

queryDerivationOutputNames
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePathName)
queryDerivationOutputNames = doReq . QueryDerivationOutputNames

queryPathFromHashPart
  :: MonadRemoteStore m
  => StorePathHashPart
  -> m StorePath
queryPathFromHashPart = doReq . QueryPathFromHashPart

queryMissing
  :: MonadRemoteStore m
  => Set DerivedPath
  -> m Missing
queryMissing = doReq . QueryMissing

optimiseStore
  :: MonadRemoteStore m
  => m ()
optimiseStore = doReq OptimiseStore

syncWithGC
  :: MonadRemoteStore m
  => m ()
syncWithGC = doReq SyncWithGC

verifyStore
  :: MonadRemoteStore m
  => CheckMode
  -> RepairMode
  -> m Bool
verifyStore check repair = doReq (VerifyStore check repair)
