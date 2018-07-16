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
import           Data.HashSet              (HashSet)
import           System.Nix.Path           (Path)

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
    status             :: !BuildStatus
  , -- | possible build error message
    error              :: !(Maybe Text)
  , -- | How many times this build was performed
    timesBuilt         :: !Integer
  , -- | If timesBuilt > 1, whether some builds did not produce the same result
    isNonDeterministic :: !Bool
    -- XXX: | startTime stopTime time_t
  } deriving (Eq, Ord, Show)

buildSuccess BuildResult{..} = status == Built || status == Substituted || status == AlreadyValid
