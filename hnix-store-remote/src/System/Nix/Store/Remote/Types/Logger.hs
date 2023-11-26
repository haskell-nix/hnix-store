module System.Nix.Store.Remote.Types.Logger
  ( Field(..)
  , Logger(..)
  , LoggerOpCode(..)
  , loggerOpCodeToInt
  , intToLoggerOpCode
  , isError
  ) where

import Data.ByteString (ByteString)
import GHC.Generics
import System.Nix.Store.Remote.Types.Activity (Activity, ActivityID, ActivityResult)
import System.Nix.Store.Remote.Types.Verbosity (Verbosity)

data Field
  = Field_LogStr ByteString
  | Field_LogInt Int
  deriving (Eq, Generic, Ord, Show)

data LoggerOpCode
  = LoggerOpCode_Next
  | LoggerOpCode_Read
  | LoggerOpCode_Write
  | LoggerOpCode_Last
  | LoggerOpCode_Error
  | LoggerOpCode_StartActivity
  | LoggerOpCode_StopActivity
  | LoggerOpCode_Result
  deriving (Eq, Generic, Ord, Show)

loggerOpCodeToInt :: LoggerOpCode -> Int
loggerOpCodeToInt = \case
  LoggerOpCode_Next -> 0x6f6c6d67
  LoggerOpCode_Read -> 0x64617461
  LoggerOpCode_Write -> 0x64617416
  LoggerOpCode_Last -> 0x616c7473
  LoggerOpCode_Error -> 0x63787470
  LoggerOpCode_StartActivity -> 0x53545254
  LoggerOpCode_StopActivity -> 0x53544f50
  LoggerOpCode_Result -> 0x52534c54

intToLoggerOpCode :: Int -> Either String LoggerOpCode
intToLoggerOpCode = \case
  0x6f6c6d67 -> Right LoggerOpCode_Next
  0x64617461 -> Right LoggerOpCode_Read
  0x64617416 -> Right LoggerOpCode_Write
  0x616c7473 -> Right LoggerOpCode_Last
  0x63787470 -> Right LoggerOpCode_Error
  0x53545254 -> Right LoggerOpCode_StartActivity
  0x53544f50 -> Right LoggerOpCode_StopActivity
  0x52534c54 -> Right LoggerOpCode_Result
  x -> Left $ "Invalid LoggerOpCode: " ++ show x

data Logger
  = Logger_Next ByteString
  | Logger_Read Int         -- data needed from source
  | Logger_Write ByteString -- data for sink
  | Logger_Last
  | Logger_Error
      { errorExitStatus :: Int
      , errorMessage :: ByteString
      }
  | Logger_StartActivity
      { startActivityID :: ActivityID
      , startActivityVerbosity :: Verbosity
      , startActivityType :: Maybe Activity
      , startActivityString :: ByteString
      , startActivityFields :: [Field]
      , startActivityParentID :: ActivityID
      }
  | Logger_StopActivity
      { stopActivityID :: ActivityID
      }
  | Logger_Result
      { resultActivityID :: ActivityID
      , resultType :: ActivityResult
      , resultFields :: [Field]
      }
  deriving (Eq, Generic, Ord, Show)

isError :: Logger -> Bool
isError Logger_Error {} = True
isError _ = False
