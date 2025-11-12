{-|
Description : Build related types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Build
  ( BuildMode(..)
  , BuildSuccessStatus(..)
  , BuildFailureStatus(..)
  , BuildResult(..)
  , BuildSuccess(..)
  , BuildFailure(..)
  ) where

import Data.Map (Map)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

import System.Nix.OutputName (OutputName)
import System.Nix.Realisation (BuildTraceKey, Realisation)

-- | Mode of the build operation
-- Keep the order of these Enums to match enums from reference implementations
-- src/libstore/store-api.hh
data BuildMode
  = BuildMode_Normal -- ^ Perform normal build
  | BuildMode_Repair -- ^ Try to repair corrupted or missing paths by re-building or re-downloading them
  | BuildMode_Check -- ^ Check if the build is reproducible (rebuild and compare to previous build)
  deriving (Bounded, Eq, Generic, Ord, Enum, Show)

-- | Successful build status
-- Keep the order of these Enums to match enums from reference implementations
-- Must be disjoint with BuildFailureStatus
data BuildSuccessStatus
  = BuildSuccessStatus_Built -- ^ Build performed successfully
  | BuildSuccessStatus_Substituted -- ^ Path substituted from cache
  | BuildSuccessStatus_AlreadyValid -- ^ Path is already valid (available in local store)
  | BuildSuccessStatus_ResolvesToAlreadyValid
  deriving (Bounded, Eq, Generic, Ord, Enum, Show)

-- | Failed build status
-- Keep the order of these Enums to match enums from reference implementations
-- Must be disjoint with BuildSuccessStatus
data BuildFailureStatus
  = BuildFailureStatus_PermanentFailure
  | BuildFailureStatus_InputRejected
  | BuildFailureStatus_OutputRejected
  | BuildFailureStatus_TransientFailure -- ^ Possibly transient build failure
  | BuildFailureStatus_CachedFailure -- ^ Obsolete
  | BuildFailureStatus_TimedOut -- ^ Build timed out
  | BuildFailureStatus_MiscFailure
  | BuildFailureStatus_DependencyFailed -- ^ Build dependency failed to build
  | BuildFailureStatus_LogLimitExceeded
  | BuildFailureStatus_NotDeterministic
  | BuildFailureStatus_NoSubstituters
  | BuildFailureStatus_HashMismatch -- ^ A certain type of OutputRejected
  deriving (Bounded, Eq, Generic, Ord, Enum, Show)

-- | Successful build result
data BuildSuccess = BuildSuccess
  { buildSuccessStatus :: BuildSuccessStatus
  , buildSuccessBuiltOutputs :: Map (BuildTraceKey OutputName) Realisation
  -- ^ Mapping of the output names to Realisations
  }
  deriving (Eq, Generic, Ord, Show)

-- | Failed build result
data BuildFailure = BuildFailure
  { buildFailureStatus :: BuildFailureStatus
  -- ^ Failure status code
  , buildFailureErrorMsg :: Text
  -- ^ Error message describing the failure
  , buildFailureIsNonDeterministic :: Bool
  -- ^ Whether the build was non-deterministic
  }
  deriving (Eq, Generic, Ord, Show)

-- | Result of the build
data BuildResult = BuildResult
  { buildResultStatus :: Either BuildFailure BuildSuccess
  -- ^ Success or failure with associated data
  , buildResultTimesBuilt :: Word64
  -- ^ How many times this build was performed
  , buildResultStartTime :: UTCTime
  -- ^ Start time of this build
  , buildResultStopTime :: UTCTime
  -- ^ Stop time of this build
  , buildResultCpuUser :: Maybe Word64
  -- ^ User CPU time in microseconds
  , buildResultCpuSystem :: Maybe Word64
  -- ^ System CPU time in microseconds
  }
  deriving (Eq, Generic, Ord, Show)
