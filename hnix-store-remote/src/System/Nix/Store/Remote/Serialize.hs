{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Description : Serialize instances for complex types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Store.Remote.Serialize where

import Data.Serialize (Serialize(..))
import Data.Text (Text)

import qualified Data.Bool
import qualified Data.Text

import System.Nix.Build (BuildMode(..), BuildStatus(..), BuildResult(..))
import System.Nix.Store.Remote.Serialize.Prim

instance Serialize Text where
  get = getText
  put = putText

instance Serialize BuildMode where
  get = getEnum
  put = putEnum

instance Serialize BuildStatus where
  get = getEnum
  put = putEnum

instance Serialize BuildResult where
  get = do
    status <- get
    errorMessage <-
      (\em -> Data.Bool.bool (Just em) Nothing (Data.Text.null em)) 
      <$> get
    timesBuilt <- getInt
    isNonDeterministic <- getBool
    startTime <- getTime
    stopTime <- getTime
    pure $ BuildResult{..}

  put BuildResult{..} = do
    put status
    case errorMessage of
      Just err -> putText err
      Nothing -> putText mempty
    putInt timesBuilt
    putBool isNonDeterministic
    putTime startTime
    putTime stopTime
