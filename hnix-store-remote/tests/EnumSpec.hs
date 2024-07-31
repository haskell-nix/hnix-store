{-# LANGUAGE OverloadedStrings #-}

module EnumSpec (spec) where

import Test.Hspec (SpecWith, Spec, describe, it, shouldBe)

import Data.ByteString (ByteString)
import Data.Word (Word64)
import System.Nix.Build (BuildMode(..), BuildStatus(..))
import System.Nix.Store.Remote.Serializer 
  ( activity
  , activityResult
  , enum
  , int
  , loggerOpCode
  , runP
  , LoggerSError
  , NixSerializer
  , SError
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
        $ ((runP enum () constr) :: Either SError ByteString)
          `shouldBe`
          (runP (int @Word64) () value)

    itE'
      :: Show a
      => NixSerializer () LoggerSError a
      -> String
      -> a
      -> Word64
      -> SpecWith ()
    itE' s name constr value =
      it name
        $ ((runP s () constr) :: Either LoggerSError ByteString)
          `shouldBe`
          (runP (int @Word64) () (value))

  describe "Enums" $ do
    describe "BuildMode enum order matches Nix" $ do
      itE "Normal" BuildMode_Normal 0
      itE "Repair" BuildMode_Repair 1
      itE "Check"  BuildMode_Check  2

    describe "BuildStatus enum order matches Nix" $ do
      itE "Built"                  BuildStatus_Built                   0
      itE "Substituted"            BuildStatus_Substituted             1
      itE "AlreadyValid"           BuildStatus_AlreadyValid            2
      itE "PermanentFailure"       BuildStatus_PermanentFailure        3
      itE "InputRejected"          BuildStatus_InputRejected           4
      itE "OutputRejected"         BuildStatus_OutputRejected          5
      itE "TransientFailure"       BuildStatus_TransientFailure        6
      itE "CachedFailure"          BuildStatus_CachedFailure           7
      itE "TimedOut"               BuildStatus_TimedOut                8
      itE "MiscFailure"            BuildStatus_MiscFailure             9
      itE "DependencyFailed"       BuildStatus_DependencyFailed       10
      itE "LogLimitExceeded"       BuildStatus_LogLimitExceeded       11
      itE "NotDeterministic"       BuildStatus_NotDeterministic       12
      itE "ResolvesToAlreadyValid" BuildStatus_ResolvesToAlreadyValid 13
      itE "NoSubstituters"         BuildStatus_NoSubstituters         14

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



