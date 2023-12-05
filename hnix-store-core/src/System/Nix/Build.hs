{-|
Description : Build related types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Build
  ( BuildMode(..)
  , BuildStatus(..)
  , buildSuccess
  , BuildResult(..)
  , OldBuildResult(..)
  ) where

import Data.Time (UTCTime)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Mode of the build operation
-- Keep the order of these Enums to match enums from reference implementations
-- src/libstore/store-api.hh
data BuildMode
  = BuildMode_Normal -- ^ Perform normal build
  | BuildMode_Repair -- ^ Try to repair corrupted or missing paths by re-building or re-downloading them
  | BuildMode_Check -- ^ Check if the build is reproducible (rebuild and compare to previous build)
  deriving (Eq, Generic, Ord, Enum, Show)

-- | Build result status
data BuildStatus =
    BuildStatus_Built -- ^ Build performed successfully
  | BuildStatus_Substituted -- ^ Path substituted from cache
  | BuildStatus_AlreadyValid -- ^ Path is already valid (available in local store)
  | BuildStatus_PermanentFailure
  | BuildStatus_InputRejected
  | BuildStatus_OutputRejected
  | BuildStatus_TransientFailure -- ^ Possibly transient build failure
  | BuildStatus_CachedFailure -- ^ Obsolete
  | BuildStatus_TimedOut -- ^ Build timed out
  | BuildStatus_MiscFailure
  | BuildStatus_DependencyFailed -- ^ Build dependency failed to build
  | BuildStatus_LogLimitExceeded
  | BuildStatus_NotDeterministic
  | BuildStatus_ResolvesToAlreadyValid
  | BuildStatus_NoSubstituters
  deriving (Eq, Generic, Ord, Enum, Show)

-- | Result of the build
data BuildResult = BuildResult
  { buildResultStatus             :: !BuildStatus
  -- ^ Build status, MiscFailure should be the default
  , buildResultErrorMessage       :: !(Maybe Text)
  -- ^ Possible build error message
  , buildResultTimesBuilt         :: !Int
  -- ^ How many times this build was performed
  , buildResultIsNonDeterministic :: !Bool
  -- ^ If timesBuilt > 1, whether some builds did not produce the same result
  , buildResultStartTime          :: !UTCTime
  -- ^ Start time of this build
  , buildResultStopTime           :: !UTCTime
  -- ^ Stop time of this build
  }
  deriving (Eq, Generic, Ord, Show)

buildSuccess :: BuildStatus -> Bool
buildSuccess x =
  x `elem`
    [ BuildStatus_Built
    , BuildStatus_Substituted
    , BuildStatus_AlreadyValid
    ]

-- | Result of the build, for protocol version <= 1.27
data OldBuildResult = OldBuildResult
  { oldBuildResultStatus       :: !BuildStatus
  -- ^ Build status, MiscFailure should be the default
  , oldBuildResultErrorMessage :: !(Maybe Text)
  -- ^ Possible build error message
  }
  deriving (Eq, Generic, Ord, Show)
