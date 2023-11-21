{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SerializeSpec (spec) where

import Data.Fixed (Uni)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Putter, runPut)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)
import Test.QuickCheck (arbitrary, forAll, suchThat)
import Test.QuickCheck.Instances ()

import qualified Data.Either
import qualified Data.HashSet
import qualified Data.Time.Clock.POSIX
import qualified System.Nix.Build

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildMode, BuildStatus)
import System.Nix.Derivation (Derivation(..))
import System.Nix.Store.Remote.Serialize (getDerivation, putDerivation)
import System.Nix.Store.Remote.Serialize.Prim

-- | Test for roundtrip using @Putter@ and @Get@ functions
roundtrips2
  :: ( Eq a
     , Show a
     )
  => Putter a
  -> Get a
  -> a
  -> Expectation
roundtrips2 putter getter =
  roundtrips
    (runPut . putter)
    (runGet getter)

-- | Test for roundtrip using @Serialize@ instance
roundtripS
  :: ( Eq a
     , Serialize a
     , Show a
     )
  => a
  -> Expectation
roundtripS =
  roundtrips
    (runPut . put)
    (runGet get)

spec :: Spec
spec = do
  describe "Prim" $ do
    prop "Int" $ roundtrips2 putInt getInt
    prop "Bool" $ roundtrips2 putBool getBool

    prop "UTCTime" $ do
      let
        -- scale to seconds and back
        toSeconds :: Int -> NominalDiffTime
        toSeconds n = realToFrac (toEnum n :: Uni)
        fromSeconds :: NominalDiffTime -> Int
        fromSeconds = (fromEnum :: Uni -> Int) . realToFrac

      roundtrips2
        (putTime . Data.Time.Clock.POSIX.posixSecondsToUTCTime . toSeconds)
        (fromSeconds . Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds <$> getTime)

  describe "Combinators" $ do
    prop "Many" $ roundtrips2 (putMany putInt) (getMany getInt)
    prop "ByteString" $ roundtrips2 putByteString getByteString
    prop "[ByteString]" $ roundtrips2 putByteStrings getByteStrings
    prop "Text" $ roundtrips2 putText getText
    prop "[Text]" $ roundtrips2 putTexts getTexts

    prop "StorePath" $ \sd ->
      roundtrips2
        (putPath sd)
        (Data.Either.fromRight undefined <$> getPath sd)

    prop "HashSet StorePath" $ \sd ->
      roundtrips2
        (putPaths sd)
        (Data.HashSet.map (Data.Either.fromRight undefined) <$> getPaths sd)

  describe "Serialize instances" $ do
    prop "Text" $ roundtripS @Text
    prop "BuildMode" $ roundtripS @BuildMode
    prop "BuildStatus" $ roundtripS @BuildStatus
    it "BuildResult" $
      forAll (arbitrary `suchThat` ((/= Just "") . System.Nix.Build.errorMessage))
      $ \br ->
          roundtripS
            -- fix time to 0 as we test UTCTime above
            $ br { System.Nix.Build.startTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                 , System.Nix.Build.stopTime  = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                 }

    prop "Derivation StorePath Text" $ \sd ->
      roundtrips2
        (putDerivation sd)
        (getDerivation sd)
        -- inputDrvs is not used in remote protocol serialization
        . (\drv -> drv { inputDrvs = mempty })

  let it' name constr value = it name $ runPut (put constr) `shouldBe` runPut (putInt value)
  describe "Build enum order matches Nix" $ do
    it' "Normal" System.Nix.Build.Normal 0
    it' "Repair" System.Nix.Build.Repair 1
    it' "Check"  System.Nix.Build.Check  2

  describe "BuildStatus enum order matches Nix" $ do
    it' "Built"                  System.Nix.Build.Built                   0
    it' "Substituted"            System.Nix.Build.Substituted             1
    it' "AlreadyValid"           System.Nix.Build.AlreadyValid            2
    it' "PermanentFailure"       System.Nix.Build.PermanentFailure        3
    it' "InputRejected"          System.Nix.Build.InputRejected           4
    it' "OutputRejected"         System.Nix.Build.OutputRejected          5
    it' "TransientFailure"       System.Nix.Build.TransientFailure        6
    it' "CachedFailure"          System.Nix.Build.CachedFailure           7
    it' "TimedOut"               System.Nix.Build.TimedOut                8
    it' "MiscFailure"            System.Nix.Build.MiscFailure             9
    it' "DependencyFailed"       System.Nix.Build.DependencyFailed       10
    it' "LogLimitExceeded"       System.Nix.Build.LogLimitExceeded       11
    it' "NotDeterministic"       System.Nix.Build.NotDeterministic       12
    it' "ResolvesToAlreadyValid" System.Nix.Build.ResolvesToAlreadyValid 13
    it' "NoSubstituters"         System.Nix.Build.NoSubstituters         14
