{-# language DataKinds           #-}
{-# language ScopedTypeVariables #-}

module StorePath where

import qualified Data.Attoparsec.Text

import           Test.Tasty.QuickCheck

import           System.Nix.StorePath
import           Arbitrary

-- | Test that Nix(OS) like paths roundtrip
prop_storePathRoundtrip :: NixLike -> NixLike -> Property
prop_storePathRoundtrip (_ :: NixLike) (NixLike x) =
  parsePath "/nix/store" (storePathToRawFilePath x) === pure x

-- | Test that any `StorePath` roundtrips
prop_storePathRoundtrip' :: StorePath -> Property
prop_storePathRoundtrip' x =
  parsePath (storePathRoot x) (storePathToRawFilePath x) === pure x

prop_storePathRoundtripParser :: NixLike -> NixLike -> Property
prop_storePathRoundtripParser (_ :: NixLike) (NixLike x) =
  Data.Attoparsec.Text.parseOnly (pathParser $ storePathRoot x) (storePathToText x) === pure x

prop_storePathRoundtripParser' :: StorePath -> Property
prop_storePathRoundtripParser' x =
  Data.Attoparsec.Text.parseOnly (pathParser $ storePathRoot x) (storePathToText x) === pure x
