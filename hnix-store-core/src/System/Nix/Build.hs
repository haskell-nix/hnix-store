{-# LANGUAGE RecordWildCards #-}
{-|
Description : Build related types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Build (
    BuildMode(..)
  , BuildStatus(..)
  , BuildResult(..)
  , buildSuccess
  ) where

import           Data.Text                 (Text)
--import           Data.HashSet              (HashSet)
--import           System.Nix.Path           (Path)

data BuildMode = Normal | Repair | Check
  deriving (Eq, Ord, Enum, Show)

data BuildStatus =
    Built
  | Substituted
  | AlreadyValid
  | PermanentFailure
  | InputRejected
  | OutputRejected
  | TransientFailure -- possibly transient
  | CachedFailure    -- no longer used
  | TimedOut
  | MiscFailure
  | DependencyFailed
  | LogLimitExceeded
  | NotDeterministic
  deriving (Eq, Ord, Enum, Show)


-- | Result of the build
data BuildResult = BuildResult
  { -- | build status, MiscFailure should be default
    _buildResult_status             :: !BuildStatus
  , -- | possible build error message
    _buildResult_error              :: !(Maybe Text)
  , -- | How many times this build was performed
    _buildResult_timesBuilt         :: !Integer
  , -- | If timesBuilt > 1, whether some builds did not produce the same result
    _buildResult_isNonDeterministic :: !Bool
    -- XXX: | startTime stopTime time_t
  } deriving (Eq, Ord, Show)

buildSuccess :: BuildResult -> Bool
buildSuccess BuildResult{..} = _buildResult_status == Built
  || _buildResult_status == Substituted
  || _buildResult_status == AlreadyValid
