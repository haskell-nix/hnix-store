{-|
Description : Garbage collection actions / options
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.GC (
    Action(..)
  , Options(..)
  , Result(..)
  ) where

import           System.Nix.Path           (PathSet)

{- Garbage collector operation:
     - ReturnLive: return the set of paths reachable from
       (i.e. in the closure of) the roots.
     - ReturnDead: return the set of paths not reachable from
       the roots.
     - DeleteDead: actually delete the latter set.
     - DeleteSpecific: delete the paths listed in
        `pathsToDelete', insofar as they are not reachable.
-}

data Action = ReturnLive | ReturnDead | DeleteDead | DeleteSpecific
  deriving (Eq, Ord, Enum, Show)

 -- | Garbage collector operation options
data Options = Options
  { -- | operation
    operation      :: !Action
    -- | If `ignoreLiveness' is set, then reachability from the roots is
    -- ignored (dangerous!).  However, the paths must still be
    -- unreferenced *within* the store (i.e., there can be no other
    -- store paths that depend on them).
  , ignoreLiveness :: !Bool
    -- | For DeleteSpecific, the paths to delete
  , pathsToDelete  :: !PathSet
  , -- | Stop after at least `maxFreed` bytes have been freed
    maxFreed       :: !Integer
  } deriving (Eq, Ord, Show)

data Result = Result
 { -- | Depending on the action, the GC roots, or the paths that would be or have been deleted
   paths      :: !PathSet
 , -- |  For ReturnDead, DeleteDead and DeleteSpecific, the number of bytes that would be or was freed
   bytesFreed :: !Integer
 } deriving (Eq, Ord, Show)

