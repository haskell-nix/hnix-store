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
  = BuildMode_Normal
  | BuildMode_Repair
  | BuildMode_Check
  deriving (Eq, Generic, Ord, Enum, Show)

data BuildStatus =
    BuildStatus_Built
  | BuildStatus_Substituted
  | BuildStatus_AlreadyValid
  | BuildStatus_PermanentFailure
  | BuildStatus_InputRejected
  | BuildStatus_OutputRejected
  | BuildStatus_TransientFailure -- possibly transient
  | BuildStatus_CachedFailure    -- no longer used
  | BuildStatus_TimedOut
  | BuildStatus_MiscFailure
  | BuildStatus_DependencyFailed
  | BuildStatus_LogLimitExceeded
  | BuildStatus_NotDeterministic
  | BuildStatus_ResolvesToAlreadyValid
  | BuildStatus_NoSubstituters
  deriving (Eq, Generic, Ord, Enum, Show)

-- | Result of the build
data BuildResult = BuildResult
  { -- | Build status, MiscFailure should be the default
    buildResultStatus             :: !BuildStatus
  , -- | Possible build error message
    buildResultErrorMessage       :: !(Maybe Text)
  , -- | How many times this build was performed
    buildResultTimesBuilt         :: !Int
  , -- | If timesBuilt > 1, whether some builds did not produce the same result
    buildResultIsNonDeterministic :: !Bool
  ,  -- Start time of this build
    buildResultStartTime          :: !UTCTime
  ,  -- Stop time of this build
    buildResultStopTime           :: !UTCTime
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
  { -- | Build status, MiscFailure should be the default
    oldBuildResultStatus       :: !BuildStatus
  , -- | Possible build error message
    oldBuildResultErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Generic, Ord, Show)
