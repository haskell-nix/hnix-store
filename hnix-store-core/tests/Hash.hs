{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Hash where

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
import           NarFormat -- TODO: Move the fixtures into a common module

spec_hash :: Spec
spec_hash = do

  describe "hashing parity with nix-store" $ do

    it "produces (base32 . sha1) of \"Hello World\" the same as the thesis" $
      shouldBe (printAsBase32 (hash @SHA1 "Hello World"))
               "s23c9fs0v32pf6bhmcph5rbqsyl5ak8a"

    -- The example in question:
    -- https://nixos.org/nixos/nix-pills/nix-store-paths.html
    it "produces same base32 as nix pill flat file example" $ do
      let exampleStr =
            "source:sha256:2bfef67de873c54551d884fdab3055d84d573e654efa79db3"
            <> "c0d7b98883f9ee3:/nix/store:myfile"
      shouldBe (printAsBase32 @PathHashAlgo (hash exampleStr))
        "xv2iccirbrvklck36f1g7vldn5v58vck"
