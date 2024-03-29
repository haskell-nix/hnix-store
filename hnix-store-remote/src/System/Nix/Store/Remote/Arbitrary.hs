-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Store.Remote.Arbitrary where

import Data.Some (Some(Some))
import System.Nix.Arbitrary ()
import System.Nix.Store.Types (RepairMode(..))
import System.Nix.Store.Remote.Types

import Test.QuickCheck (Arbitrary(..), oneof, suchThat)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

deriving via GenericArbitrary CheckMode
  instance Arbitrary CheckMode

deriving via GenericArbitrary SubstituteMode
  instance Arbitrary SubstituteMode

deriving via GenericArbitrary ProtoStoreConfig
  instance Arbitrary ProtoStoreConfig

deriving via GenericArbitrary ProtoVersion
  instance Arbitrary ProtoVersion

deriving via GenericArbitrary StoreText
  instance Arbitrary StoreText

-- * Logger

deriving via GenericArbitrary Activity
  instance Arbitrary Activity

deriving via GenericArbitrary ActivityID
  instance Arbitrary ActivityID

deriving via GenericArbitrary ActivityResult
  instance Arbitrary ActivityResult

deriving via GenericArbitrary Field
  instance Arbitrary Field

instance Arbitrary Trace where
  arbitrary = do
    -- we encode 0 position as Nothing
    tracePosition <- arbitrary `suchThat` (/= Just 0)
    traceHint <- arbitrary

    pure Trace{..}

deriving via GenericArbitrary BasicError
  instance Arbitrary BasicError

instance Arbitrary ErrorInfo where
  arbitrary = do
    errorInfoLevel <- arbitrary
    errorInfoMessage <- arbitrary
    -- we encode 0 position as Nothing
    errorInfoPosition <- arbitrary `suchThat` (/= Just 0)
    errorInfoTraces <- arbitrary

    pure ErrorInfo{..}

deriving via GenericArbitrary LoggerOpCode
  instance Arbitrary LoggerOpCode

deriving via GenericArbitrary Logger
  instance Arbitrary Logger

deriving via GenericArbitrary Verbosity
  instance Arbitrary Verbosity

-- * GC

deriving via GenericArbitrary GCAction
  instance Arbitrary GCAction

deriving via GenericArbitrary GCOptions
  instance Arbitrary GCOptions

-- * Handshake

deriving via GenericArbitrary WorkerMagic
  instance Arbitrary WorkerMagic

deriving via GenericArbitrary TrustedFlag
  instance Arbitrary TrustedFlag

-- * Worker protocol

deriving via GenericArbitrary WorkerOp
  instance Arbitrary WorkerOp

-- ** Request

instance Arbitrary (Some StoreRequest) where
  arbitrary = oneof
    [ Some <$> (AddToStore <$> arbitrary <*> arbitrary <*> arbitrary <*> pure RepairMode_DontRepair)
    , Some <$> (AddTextToStore <$> arbitrary <*> arbitrary <*> pure RepairMode_DontRepair)
    , Some <$> (AddSignatures <$> arbitrary <*> arbitrary)
    , Some . AddIndirectRoot  <$> arbitrary
    , Some . AddTempRoot <$> arbitrary
    , Some <$> (BuildPaths <$> arbitrary <*> arbitrary)
    , Some <$> (BuildDerivation <$> arbitrary <*> arbitrary <*> arbitrary)
    , Some . CollectGarbage <$> arbitrary
    , Some . EnsurePath <$> arbitrary
    , pure $ Some FindRoots
    , Some . IsValidPath <$> arbitrary
    , Some . NarFromPath <$> arbitrary
    , Some <$> (QueryValidPaths <$> arbitrary <*> arbitrary)
    , pure $ Some QueryAllValidPaths
    , Some . QuerySubstitutablePaths <$> arbitrary
    , Some . QueryPathInfo <$> arbitrary
    , Some . QueryReferrers <$> arbitrary
    , Some . QueryValidDerivers <$> arbitrary
    , Some . QueryDerivationOutputs <$> arbitrary
    , Some . QueryDerivationOutputNames <$> arbitrary
    , Some . QueryPathFromHashPart <$> arbitrary
    , Some . QueryMissing <$> arbitrary
    , pure $ Some OptimiseStore
    , pure $ Some SyncWithGC
    , Some <$> (VerifyStore <$> arbitrary <*> arbitrary)
    ]

-- ** Reply

deriving via GenericArbitrary SuccessCodeReply
  instance Arbitrary SuccessCodeReply

deriving via GenericArbitrary GCResult
  instance Arbitrary GCResult

deriving via GenericArbitrary GCRoot
  instance Arbitrary GCRoot

deriving via GenericArbitrary Missing
  instance Arbitrary Missing
