-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Store.Remote.Arbitrary where

import Data.Some (Some(Some))
import System.Nix.Arbitrary ()
import System.Nix.Store.Remote.Types

import Test.QuickCheck (Arbitrary(..), oneof)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

deriving via GenericArbitrary CheckMode
  instance Arbitrary CheckMode

deriving via GenericArbitrary SubstituteMode
  instance Arbitrary SubstituteMode

deriving via GenericArbitrary TestStoreConfig
  instance Arbitrary TestStoreConfig

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

deriving via GenericArbitrary Trace
  instance Arbitrary Trace

deriving via GenericArbitrary BasicError
  instance Arbitrary BasicError

deriving via GenericArbitrary ErrorInfo
  instance Arbitrary ErrorInfo

deriving via GenericArbitrary LoggerOpCode
  instance Arbitrary LoggerOpCode

deriving via GenericArbitrary Logger
  instance Arbitrary Logger

deriving via GenericArbitrary Verbosity
  instance Arbitrary Verbosity

instance Arbitrary (Some StoreRequest) where
  arbitrary = oneof
    [ Some <$> (AddToStore <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
    , Some <$> (AddTextToStore <$> arbitrary <*> arbitrary <*> arbitrary)
    , Some <$> (AddSignatures <$> arbitrary <*> arbitrary)
    , Some . AddIndirectRoot  <$> arbitrary
    , Some . AddTempRoot <$> arbitrary
    , Some <$> (BuildPaths <$> arbitrary <*> arbitrary)
    , Some <$> (BuildDerivation <$> arbitrary <*> arbitrary <*> arbitrary)
    , Some . EnsurePath <$> arbitrary
    , pure $ Some FindRoots
    , Some . IsValidPath <$> arbitrary
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
