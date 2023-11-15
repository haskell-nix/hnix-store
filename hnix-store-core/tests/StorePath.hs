{-# language DataKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}

module StorePath where

import qualified Data.Attoparsec.Text

import           Test.Tasty.QuickCheck

import           System.Nix.StorePath

-- | Test @StorePath@ roundtrips using @parsePath@
prop_storePathRoundtrip :: StoreDir -> StorePath -> Property
prop_storePathRoundtrip storeDir x =
  parsePath storeDir (storePathToRawFilePath storeDir x) === pure x

-- | Test @StorePath@ roundtrips using @pathParser@
prop_storePathRoundtripParser :: StoreDir -> StorePath -> Property
prop_storePathRoundtripParser storeDir x =
  Data.Attoparsec.Text.parseOnly (pathParser storeDir) (storePathToText storeDir x) === pure x
