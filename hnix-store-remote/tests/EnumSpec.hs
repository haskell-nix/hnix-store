{-# LANGUAGE OverloadedStrings #-}

module EnumSpec (spec) where

import Test.Hspec (SpecWith, Spec, describe, it, shouldBe)

import Data.ByteString (ByteString)
import Data.Word (Word64)
import System.Nix.Build (BuildMode(..), BuildSuccessStatus(..), BuildFailureStatus(..))
import System.Nix.Store.Remote.Serializer
  ( activity
  , activityResult
  , enum
  , int
  , loggerOpCode
  , runP
  , LoggerSError
  , NixSerializer
  )
import System.Nix.Store.Remote.Types

spec :: Spec
spec = do
  let
    itE
      :: ( Bounded a
         , Enum a
         , Show a
         )
      => String
      -> a
      -> Word64
      -> SpecWith ()
    itE name constr value =
      it name
        $ ((runP enum constr) :: ByteString)
          `shouldBe`
          (runP (int @Word64) value)

    itE'
      :: Show a
      => NixSerializer LoggerSError a
      -> String
      -> a
      -> Word64
      -> SpecWith ()
    itE' s name constr value =
      it name
        $ ((runP s constr) :: ByteString)
          `shouldBe`
          (runP (int @Word64) (value))

  describe "Enums" $ do
    describe "BuildMode enum order matches Nix" $ do
      itE "Normal" BuildMode_Normal 0
      itE "Repair" BuildMode_Repair 1
      itE "Check"  BuildMode_Check  2

    describe "BuildSuccessStatus enum order matches Nix" $ do
      itE "Built"                  BuildSuccessStatus_Built                   0
      itE "Substituted"            BuildSuccessStatus_Substituted             1
      itE "AlreadyValid"           BuildSuccessStatus_AlreadyValid            2
      itE "ResolvesToAlreadyValid" BuildSuccessStatus_ResolvesToAlreadyValid  3

    describe "BuildFailureStatus enum order matches Nix" $ do
      itE "PermanentFailure"       BuildFailureStatus_PermanentFailure        0
      itE "InputRejected"          BuildFailureStatus_InputRejected           1
      itE "OutputRejected"         BuildFailureStatus_OutputRejected          2
      itE "TransientFailure"       BuildFailureStatus_TransientFailure        3
      itE "CachedFailure"          BuildFailureStatus_CachedFailure           4
      itE "TimedOut"               BuildFailureStatus_TimedOut                5
      itE "MiscFailure"            BuildFailureStatus_MiscFailure             6
      itE "DependencyFailed"       BuildFailureStatus_DependencyFailed        7
      itE "LogLimitExceeded"       BuildFailureStatus_LogLimitExceeded        8
      itE "NotDeterministic"       BuildFailureStatus_NotDeterministic        9
      itE "NoSubstituters"         BuildFailureStatus_NoSubstituters         10
      itE "HashMismatch"           BuildFailureStatus_HashMismatch           11

    describe "GCAction enum order matches Nix" $ do
      itE "ReturnLive"     GCAction_ReturnLive     0
      itE "ReturnDead"     GCAction_ReturnDead     1
      itE "DeleteDead"     GCAction_DeleteDead     2
      itE "DeleteSpecific" GCAction_DeleteSpecific 3

    describe "Logger" $ do
      let itA = itE' activity
      describe "Activity enum order matches Nix" $ do
        itA "CopyPath"      Activity_CopyPath      100
        itA "FileTransfer"  Activity_FileTransfer  101
        itA "Realise"       Activity_Realise       102
        itA "CopyPaths"     Activity_CopyPaths     103
        itA "Builds"        Activity_Builds        104
        itA "Build"         Activity_Build         105
        itA "OptimiseStore" Activity_OptimiseStore 106
        itA "VerifyPaths"   Activity_VerifyPaths   107
        itA "Substitute"    Activity_Substitute    108
        itA "QueryPathInfo" Activity_QueryPathInfo 109
        itA "PostBuildHook" Activity_PostBuildHook 110
        itA "BuildWaiting"  Activity_BuildWaiting  111

      let itR = itE' activityResult
      describe "ActivityResult enum order matches Nix" $ do
        itR "FileLinked"       ActivityResult_FileLinked       100
        itR "BuildLogLine"     ActivityResult_BuildLogLine     101
        itR "UnstrustedPath"   ActivityResult_UnstrustedPath   102
        itR "CorruptedPath"    ActivityResult_CorruptedPath    103
        itR "SetPhase"         ActivityResult_SetPhase         104
        itR "Progress"         ActivityResult_Progress         105
        itR "SetExpected"      ActivityResult_SetExpected      106
        itR "PostBuildLogLine" ActivityResult_PostBuildLogLine 107


      let itL = itE' loggerOpCode
      describe "LoggerOpCode matches Nix" $ do
        itL "Next"          LoggerOpCode_Next          0x6f6c6d67
        itL "Read"          LoggerOpCode_Read          0x64617461
        itL "Write"         LoggerOpCode_Write         0x64617416
        itL "Last"          LoggerOpCode_Last          0x616c7473
        itL "Error"         LoggerOpCode_Error         0x63787470
        itL "StartActivity" LoggerOpCode_StartActivity 0x53545254
        itL "StopActivity"  LoggerOpCode_StopActivity  0x53544f50
        itL "Result"        LoggerOpCode_Result        0x52534c54

      describe "Verbosity enum order matches Nix" $ do
        itE "Error"     Verbosity_Error     0
        itE "Warn"      Verbosity_Warn      1
        itE "Notice"    Verbosity_Notice    2
        itE "Info"      Verbosity_Info      3
        itE "Talkative" Verbosity_Talkative 4
        itE "Chatty"    Verbosity_Chatty    5
        itE "Debug"     Verbosity_Debug     6
        itE "Vomit"     Verbosity_Vomit     7

      describe "WorkerOp enum order matches Nix" $ do
        itE "IsValidPath"           WorkerOp_IsValidPath            1
        itE "BuildPathsWithResults" WorkerOp_BuildPathsWithResults 46



