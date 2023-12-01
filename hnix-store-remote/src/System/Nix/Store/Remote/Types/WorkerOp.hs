module System.Nix.Store.Remote.Types.WorkerOp 
  ( WorkerOp(..)
  ) where

-- | Worker opcode
--
-- This type has gaps filled in so that the GHC builtin
-- Enum instance lands on the right values.
data WorkerOp
  = WorkerOp_Reserved_0__ -- 0
  | WorkerOp_IsValidPath -- 1
  | WorkerOp_Reserved_2__ -- 2
  | WorkerOp_HasSubstitutes -- 3
  | WorkerOp_QueryPathHash -- 4 // obsolete
  | WorkerOp_QueryReferences --  5 // obsolete
  | WorkerOp_QueryReferrers --  6
  | WorkerOp_AddToStore --  7
  | WorkerOp_AddTextToStore --  8 // obsolete since 1.25, Nix 3.0. Use wopAddToStore
  | WorkerOp_BuildPaths --  9
  | WorkerOp_EnsurePath --  10 0xa
  | WorkerOp_AddTempRoot --  11 0xb
  | WorkerOp_AddIndirectRoot --  12 0xc
  | WorkerOp_SyncWithGC --  13 0xd
  | WorkerOp_FindRoots --  14 0xe
  | WorkerOp_Reserved_15__ -- 15 0xf
  | WorkerOp_ExportPath --  16 0x10 // obsolete
  | WorkerOp_Reserved_17__ -- 17 0x11
  | WorkerOp_QueryDeriver --  18 0x12 // obsolete
  | WorkerOp_SetOptions --  19 0x13
  | WorkerOp_CollectGarbage --  20 0x14
  | WorkerOp_QuerySubstitutablePathInfo --  21 0x15
  | WorkerOp_QueryDerivationOutputs --  22 0x16 // obsolete
  | WorkerOp_QueryAllValidPaths --  23 0x17
  | WorkerOp_QueryFailedPaths --  24 0x18
  | WorkerOp_ClearFailedPaths --  25 0x19
  | WorkerOp_QueryPathInfo --  26 0x1a
  | WorkerOp_ImportPaths --  27 0x1b // obsolete
  | WorkerOp_QueryDerivationOutputNames --  28 0x1c // obsolete
  | WorkerOp_QueryPathFromHashPart --  29 0x1d
  | WorkerOp_QuerySubstitutablePathInfos --  30 0x1e
  | WorkerOp_QueryValidPaths --  31 0x1f
  | WorkerOp_QuerySubstitutablePaths --  32 0x20
  | WorkerOp_QueryValidDerivers --  33 0x21
  | WorkerOp_OptimiseStore --  34 0x22
  | WorkerOp_VerifyStore --  35 0x23
  | WorkerOp_BuildDerivation --  36 0x24
  | WorkerOp_AddSignatures --  37 0x25
  | WorkerOp_NarFromPath --  38 0x26
  | WorkerOp_AddToStoreNar --  39 0x27
  | WorkerOp_QueryMissing --  40 0x28
  | WorkerOp_QueryDerivationOutputMap --  41 0x29
  | WorkerOp_RegisterDrvOutput --  42 0x2a
  | WorkerOp_QueryRealisation --  43 0x2b
  | WorkerOp_AddMultipleToStore --  44 0x2c
  | WorkerOp_AddBuildLog --  45 0x2d
  | WorkerOp_BuildPathsWithResults --  46 0x2e
  deriving (Bounded, Eq, Enum, Ord, Show, Read)
