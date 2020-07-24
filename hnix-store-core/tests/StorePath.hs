{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module StorePath where

import qualified Data.Attoparsec.Text.Lazy

import           Test.Tasty.QuickCheck

import           System.Nix.StorePath
import           Arbitrary

-- | Test that Nix(OS) like paths roundtrip
prop_storePathRoundtrip (_ :: NixLike) = \(NixLike x) ->
  (parsePath "/nix/store" $ storePathToRawFilePath x) === Right x

-- | Test that any `StorePath` roundtrips
prop_storePathRoundtrip' x =
  (parsePath (storePathRoot x) $ storePathToRawFilePath x) === Right x

prop_storePathRoundtripParser (_ :: NixLike) = \(NixLike x) ->
  (Data.Attoparsec.Text.Lazy.parseOnly (pathParser (storePathRoot x))
    $ storePathToText x) === Right x

prop_storePathRoundtripParser' x =
  (Data.Attoparsec.Text.Lazy.parseOnly (pathParser (storePathRoot x))
    $ storePathToText x) === Right x
