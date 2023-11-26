module System.Nix.Store.Remote.Types.Activity
  ( Activity(..)
  , ActivityID(..)
  , ActivityResult(..)
  ) where

import GHC.Generics

-- | Type of the activity
--
-- We don't have Activity_Unknown here
-- as we can do @Maybe Activity@ and @Nothing@
-- corresponding to Unknown (which has 0 value)
--
-- Rest of the values are offset by @(+100)@
-- on the wire, i.e.:
--
--   * @Activity_CopyPath = 100@
--   * @Activity_BuildWaiting = 111@
data Activity
  = Activity_CopyPath
  | Activity_FileTransfer
  | Activity_Realise
  | Activity_CopyPaths
  | Activity_Builds
  | Activity_Build
  | Activity_OptimiseStore
  | Activity_VerifyPaths
  | Activity_Substitute
  | Activity_QueryPathInfo
  | Activity_PostBuildHook
  | Activity_BuildWaiting
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)

-- | Numeric ID of the activity
newtype ActivityID = ActivityID { unActivityID :: Int }
  deriving (Eq, Generic, Ord, Show)

-- | Result of some activity
--
-- The values are offset by @(+100)@
-- on the wire, i.e.:
--
--   * @ActivityResult_FileLinked = 100@
--   * @ActivityResult_PostBuildLogLine = 107@
data ActivityResult
  = ActivityResult_FileLinked
  | ActivityResult_BuildLogLine
  | ActivityResult_UnstrustedPath
  | ActivityResult_CorruptedPath
  | ActivityResult_SetPhase
  | ActivityResult_Progress
  | ActivityResult_SetExpected
  | ActivityResult_PostBuildLogLine
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)
