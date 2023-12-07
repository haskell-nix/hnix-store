{-|
Description : Garbage collection actions / options
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Store.Remote.Types.GC (
    GCAction(..)
  , GCOptions(..)
  , GCResult(..)
  ) where

import Data.HashSet (HashSet)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Nix.StorePath (StorePath)

-- | Garbage collection action
data GCAction
  = GCAction_ReturnLive -- ^ Return the set of paths reachable from roots (closure)
  | GCAction_ReturnDead -- ^ Return unreachable paths
  | GCAction_DeleteDead -- ^ Delete unreachable paths
  | GCAction_DeleteSpecific -- ^ Delete specified paths
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)

-- | Garbage collector operation options
data GCOptions = GCOptions
  { -- | Operation
    gcOptionsOperation :: GCAction
    -- | If set, then reachability from the roots is ignored (unused)
  , gcOptionsIgnoreLiveness :: Bool
    -- | Paths to delete for @GCAction_DeleteSpecific@
  , gcOptionsPathsToDelete :: HashSet StorePath
    -- | Stop after `gcOptions_maxFreed` bytes have been freed
  , gcOptionsMaxFreed :: Word64
  } deriving (Eq, Generic, Ord, Show)

-- | Result of the garbage collection operation
data GCResult = GCResult
 { -- | Depending on the action, the GC roots,
   -- or the paths that would be or have been deleted
   gcResultDeletedPaths :: HashSet StorePath
   -- | The number of bytes that would be or was freed for
   --
   --      - @GCAction_ReturnDead@
   --      - @GCAction_DeleteDead@
   --      - @GCAction_DeleteSpecific@
 , gcResultBytesFreed :: Word64
 } deriving (Eq, Generic, Ord, Show)
