-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-|
Description : Build related types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Build
  ( BuildMode(..)
  , BuildStatus(..)
  , BuildResult(..)
  , buildSuccess
  ) where

import Data.Time (UTCTime)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck.Instances ()

-- keep the order of these Enums to match enums from reference implementations
-- src/libstore/store-api.hh
data BuildMode = Normal | Repair | Check
  deriving (Eq, Generic, Ord, Enum, Show)
  deriving Arbitrary via GenericArbitrary BuildMode

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
  deriving (Eq, Generic, Ord, Enum, Show)
  deriving Arbitrary via GenericArbitrary BuildStatus

-- | Result of the build
data BuildResult = BuildResult
  { -- | build status, MiscFailure should be default
    status             :: !BuildStatus
  , -- | possible build error message
    errorMessage       :: !(Maybe Text)
  , -- | How many times this build was performed
    timesBuilt         :: !Integer
  , -- | If timesBuilt > 1, whether some builds did not produce the same result
    isNonDeterministic :: !Bool
  ,  -- Start time of this build
    startTime          :: !UTCTime
  ,  -- Stop time of this build
    stopTime           :: !UTCTime
  }
  deriving (Eq, Generic, Ord, Show)
  deriving Arbitrary via GenericArbitrary BuildResult

buildSuccess :: BuildResult -> Bool
buildSuccess BuildResult {..} =
  status `elem` [Built, Substituted, AlreadyValid]
