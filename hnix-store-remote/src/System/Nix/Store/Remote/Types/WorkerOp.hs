module System.Nix.Store.Remote.Types.WorkerOp 
  ( WorkerOp(..)
  ) where

-- | Worker opcode
--
-- This type has gaps filled in so that the GHC builtin
-- Enum instance lands on the right values.
data WorkerOp
  = Reserved_0__ -- 0
  | IsValidPath -- 1
  | Reserved_2__ -- 2
  | HasSubstitutes -- 3
  | QueryPathHash -- 4 // obsolete
  | QueryReferences --  5 // obsolete
  | QueryReferrers --  6
  | AddToStore --  7
  | AddTextToStore --  8 // obsolete since 1.25, Nix 3.0. Use wopAddToStore
  | BuildPaths --  9
  | EnsurePath --  10 0xa
  | AddTempRoot --  11 0xb
  | AddIndirectRoot --  12 0xc
  | SyncWithGC --  13 0xd
  | FindRoots --  14 0xe
  | Reserved_15__ -- 15 0xf
  | ExportPath --  16 0x10 // obsolete
  | Reserved_17__ -- 17 0x11
  | QueryDeriver --  18 0x12 // obsolete
  | SetOptions --  19 0x13
  | CollectGarbage --  20 0x14
  | QuerySubstitutablePathInfo --  21 0x15
  | QueryDerivationOutputs --  22 0x16 // obsolete
  | QueryAllValidPaths --  23 0x17
  | QueryFailedPaths --  24 0x18
  | ClearFailedPaths --  25 0x19
  | QueryPathInfo --  26 0x1a
  | ImportPaths --  27 0x1b // obsolete
  | QueryDerivationOutputNames --  28 0x1c // obsolete
  | QueryPathFromHashPart --  29 0x1d
  | QuerySubstitutablePathInfos --  30 0x1e
  | QueryValidPaths --  31 0x1f
  | QuerySubstitutablePaths --  32 0x20
  | QueryValidDerivers --  33 0x21
  | OptimiseStore --  34 0x22
  | VerifyStore --  35 0x23
  | BuildDerivation --  36 0x24
  | AddSignatures --  37 0x25
  | NarFromPath --  38 0x26
  | AddToStoreNar --  39 0x27
  | QueryMissing --  40 0x28
  | QueryDerivationOutputMap --  41 0x29
  | RegisterDrvOutput --  42 0x2a
  | QueryRealisation --  43 0x2b
  | AddMultipleToStore --  44 0x2c
  | AddBuildLog --  45 0x2d
  | BuildPathsWithResults --  46 0x2e
  deriving (Bounded, Eq, Enum, Ord, Show, Read)
