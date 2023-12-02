{-# LANGUAGE OverloadedStrings #-}

module SerializeSpec (spec) where

import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Putter, runPut)
import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)
import Test.QuickCheck (arbitrary, forAll, suchThat)

import qualified Data.Either
import qualified Data.HashSet
import qualified System.Nix.Build

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildMode(..), BuildResult, BuildStatus(..))
import System.Nix.Derivation (Derivation(inputDrvs))
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serialize (getDerivation, putDerivation)
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Types

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
spec = parallel $ do
  describe "Prim" $ do
    prop "Int" $ roundtrips2 putInt (getInt @Int)
    prop "Bool" $ roundtrips2 putBool getBool
    prop "ByteString" $ roundtrips2 putByteString getByteString

  describe "Combinators" $ do
    prop "Many" $ roundtrips2 (putMany putInt) (getMany (getInt @Int))
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
      $ roundtripS @BuildResult

    prop "ProtoVersion" $ roundtripS @ProtoVersion

    prop "Derivation StorePath Text" $ \sd ->
      roundtrips2
        (putDerivation sd)
        (getDerivation sd)
        -- inputDrvs is not used in remote protocol serialization
        . (\drv -> drv { inputDrvs = mempty })

    describe "Logger" $ do
      prop "Activity" $ roundtripS @Activity
      prop "ActivityID" $ roundtripS @ActivityID
      prop "Activity" $ roundtripS @Activity
      prop "Field" $ roundtripS @Field
      prop "Trace"
        $ forAll (arbitrary `suchThat` ((/= Just 0) . tracePosition))
        $ roundtripS @Trace
      prop "BasicError" $ roundtripS @BasicError
      prop "ErrorInfo"
        $ forAll (arbitrary
                  `suchThat`
                    (\ErrorInfo{..}
                        -> errorInfoPosition /= Just 0
                           && all ((/= Just 0) . tracePosition) errorInfoTraces
                    )
                 )
        $ roundtripS @ErrorInfo
      prop "LoggerOpCode" $ roundtripS @LoggerOpCode
      prop "Verbosity" $ roundtripS @Verbosity

  describe "Enums" $ do
    let it' name constr value = it name $ runPut (put constr) `shouldBe` runPut (putInt @Int value)
    describe "BuildMode enum order matches Nix" $ do
      it' "Normal" BuildMode_Normal 0
      it' "Repair" BuildMode_Repair 1
      it' "Check"  BuildMode_Check  2

    describe "BuildStatus enum order matches Nix" $ do
      it' "Built"                  BuildStatus_Built                   0
      it' "Substituted"            BuildStatus_Substituted             1
      it' "AlreadyValid"           BuildStatus_AlreadyValid            2
      it' "PermanentFailure"       BuildStatus_PermanentFailure        3
      it' "InputRejected"          BuildStatus_InputRejected           4
      it' "OutputRejected"         BuildStatus_OutputRejected          5
      it' "TransientFailure"       BuildStatus_TransientFailure        6
      it' "CachedFailure"          BuildStatus_CachedFailure           7
      it' "TimedOut"               BuildStatus_TimedOut                8
      it' "MiscFailure"            BuildStatus_MiscFailure             9
      it' "DependencyFailed"       BuildStatus_DependencyFailed       10
      it' "LogLimitExceeded"       BuildStatus_LogLimitExceeded       11
      it' "NotDeterministic"       BuildStatus_NotDeterministic       12
      it' "ResolvesToAlreadyValid" BuildStatus_ResolvesToAlreadyValid 13
      it' "NoSubstituters"         BuildStatus_NoSubstituters         14

    describe "GCAction enum order matches Nix" $ do
      it' "ReturnLive"     GCAction_ReturnLive     0
      it' "ReturnDead"     GCAction_ReturnDead     1
      it' "DeleteDead"     GCAction_DeleteDead     2
      it' "DeleteSpecific" GCAction_DeleteSpecific 3

    describe "Logger" $ do
      describe "Activity enum order matches Nix" $ do
        it' "CopyPath"      Activity_CopyPath      100
        it' "FileTransfer"  Activity_FileTransfer  101
        it' "Realise"       Activity_Realise       102
        it' "CopyPaths"     Activity_CopyPaths     103
        it' "Builds"        Activity_Builds        104
        it' "Build"         Activity_Build         105
        it' "OptimiseStore" Activity_OptimiseStore 106
        it' "VerifyPaths"   Activity_VerifyPaths   107
        it' "Substitute"    Activity_Substitute    108
        it' "QueryPathInfo" Activity_QueryPathInfo 109
        it' "PostBuildHook" Activity_PostBuildHook 110
        it' "BuildWaiting"  Activity_BuildWaiting  111

      describe "ActivityResult enum order matches Nix" $ do
        it' "FileLinked"       ActivityResult_FileLinked       100
        it' "BuildLogLine"     ActivityResult_BuildLogLine     101
        it' "UnstrustedPath"   ActivityResult_UnstrustedPath   102
        it' "CorruptedPath"    ActivityResult_CorruptedPath    103
        it' "SetPhase"         ActivityResult_SetPhase         104
        it' "Progress"         ActivityResult_Progress         105
        it' "SetExpected"      ActivityResult_SetExpected      106
        it' "PostBuildLogLine" ActivityResult_PostBuildLogLine 107

      describe "LoggerOpCode matches Nix" $ do
        it' "Next"          LoggerOpCode_Next          0x6f6c6d67
        it' "Read"          LoggerOpCode_Read          0x64617461
        it' "Write"         LoggerOpCode_Write         0x64617416
        it' "Last"          LoggerOpCode_Last          0x616c7473
        it' "Error"         LoggerOpCode_Error         0x63787470
        it' "StartActivity" LoggerOpCode_StartActivity 0x53545254
        it' "StopActivity"  LoggerOpCode_StopActivity  0x53544f50
        it' "Result"        LoggerOpCode_Result        0x52534c54

      describe "Verbosity enum order matches Nix" $ do
        it' "Error"     Verbosity_Error     0
        it' "Warn"      Verbosity_Warn      1
        it' "Notice"    Verbosity_Notice    2
        it' "Info"      Verbosity_Info      3
        it' "Talkative" Verbosity_Talkative 4
        it' "Chatty"    Verbosity_Chatty    5
        it' "Debug"     Verbosity_Debug     6
        it' "Vomit"     Verbosity_Vomit     7
