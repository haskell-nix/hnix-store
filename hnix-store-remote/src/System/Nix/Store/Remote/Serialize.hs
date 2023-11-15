{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Description : Serialize instances for complex types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Store.Remote.Serialize where

import Data.Text
import Data.Serialize (Serialize(..))

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
  get =
    BuildResult
      <$> get
      -- TODO(srk): fishy
      <*> (Just <$> get)
      <*> getInt
      <*> getBool
      <*> getTime
      <*> getTime
  put = undefined
