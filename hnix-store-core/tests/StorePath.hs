{-# language DataKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}

module StorePath where

import qualified Data.Attoparsec.Text

import           Test.Tasty.QuickCheck

import           System.Nix.StorePath
import           Arbitrary

-- | Test that Nix(OS) like paths roundtrip
prop_storePathRoundtrip :: StoreDir -> NixLike -> NixLike -> Property
prop_storePathRoundtrip storeDir (_ :: NixLike) (NixLike x) =
  parsePath storeDir (storePathToRawFilePath storeDir x) === pure x

-- | Test that any `StorePath` roundtrips
prop_storePathRoundtrip' :: StoreDir -> StorePath -> Property
prop_storePathRoundtrip' storeDir x =
  parsePath storeDir (storePathToRawFilePath storeDir x) === pure x

prop_storePathRoundtripParser :: StoreDir -> NixLike -> NixLike -> Property
prop_storePathRoundtripParser storeDir (_ :: NixLike) (NixLike x) =
  Data.Attoparsec.Text.parseOnly (pathParser storeDir) (storePathToText storeDir x) === pure x

prop_storePathRoundtripParser' :: StoreDir -> StorePath -> Property
prop_storePathRoundtripParser' storeDir x =
  Data.Attoparsec.Text.parseOnly (pathParser storeDir) (storePathToText storeDir x) === pure x
