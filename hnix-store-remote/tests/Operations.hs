{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Operations where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe

import           Data.Proxy
import           Data.Text.Encoding    ( encodeUtf8 )
import           Data.Text.Lazy       as TL
import           Data.Text.Lazy.Encoding as TL

import           System.Nix.Hash
import           System.Nix.Nar
import           System.Nix.StorePath
import           System.Nix.Store.Remote
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util
import           System.Nix.Util

import           Test.Tasty                  as T
import           Test.Tasty.Hspec
import qualified Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck
import           Text.Read                   (readMaybe)

spec_addToStore :: Spec
spec_addToStore = do

  describe "addToStore remote operation" $ do

    it "uploads correctly" $ do
      let name = fromJust $ makeStorePathName "test-recursive-add"
      let srcPath = "./tests/data/add-recursive"
      let recursive = True
      let filter path = False -- not used anyway.
      let repair = False
      res <- runStore $ addToStore @'SHA256 name srcPath recursive filter repair
      res `shouldBe` (Right "/nix/store/0mbh3xdb9fkqb2i3iwv6hhz7qiicca83-test-recursive-add",[Last])

