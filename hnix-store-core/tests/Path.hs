{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Path where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Exception           (bracket)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BSL
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           System.Directory            (removeFile)
import           System.IO.Temp              (withSystemTempFile, writeSystemTempFile)
import qualified System.IO                   as IO -- (hGetContents, hPutStr, openFile)
import qualified System.Process              as P
import           Test.Tasty                  as T
import           Test.Tasty.Hspec
import qualified Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck
import           Text.Read                   (readMaybe)

import           System.Nix.Hash
import           System.Nix.Path
import           System.Nix.Internal.Hash
import           System.Nix.Internal.Path
import           NarFormat -- TODO: Move the fixtures into a common module

spec_path :: Spec
spec_path = do

  describe "path operations" $ do

    it "makeStorePath hashes correctly" $
      makeStorePath "text" (PathName "lal") (hash @MD5 "Hello World") (Settings "/nix/store") `shouldBe` "/nix/store/vsfi9phi6a2hvvvihyh48jn8xh9ld5ax-lal"

    it "store path for text matches real world test scenario" $
      storePathForText (PathName "lal") ("Hello World") [] (Settings "/run/user/1000/test-nix-store-a256230bc88fe520/store") `shouldBe` "/run/user/1000/test-nix-store-a256230bc88fe520/store/3v0g8si7h0as1nqdanymv2zh2gagbl4f-lal"

    it "parses valid path" $
      parsePath "/nix/store/vsfi9phi6a2hvvvihyh48jn8xh9ld5ax-lal" `shouldBe` (Just (Path (Digest "vsfi9phi6a2hvvvihyh48jn8xh9ld5ax") (PathName "lal")))

    it "fails on invalid name" $
      parsePath "/st/hash-$%^^#" `shouldBe` Nothing

    it "parses store" $
      parseStore "/nix/store/vsfi9phi6a2hvvvihyh48jn8xh9ld5ax-lal" `shouldBe` "/nix/store"
