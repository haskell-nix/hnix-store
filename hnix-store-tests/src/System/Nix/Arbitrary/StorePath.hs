{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.StorePath where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Crypto.Hash (MD5, SHA1, SHA256, SHA512)
import qualified Data.ByteString.Char8
import qualified Data.Text
import System.Nix.StorePath (StoreDir(..)
  , StorePath
  , StorePathName
  , StorePathHashPart
  )
import qualified System.Nix.StorePath

import Test.QuickCheck (Arbitrary(arbitrary), choose, elements, oneof, vectorOf)

instance Arbitrary StoreDir where
  arbitrary =
    StoreDir
    . (Data.ByteString.Char8.singleton '/' <>) -- TODO(srk): nasty, see #237
    . Data.ByteString.Char8.pack <$> arbitrary

instance Arbitrary StorePath where
  arbitrary =
    liftA2 System.Nix.StorePath.unsafeMakeStorePath
      arbitrary
      arbitrary

instance Arbitrary StorePathName where
  arbitrary =
      either undefined id
    . System.Nix.StorePath.mkStorePathName
    . Data.Text.pack <$> ((:) <$> s1 <*> limited sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1 = elements $ alphanum <> "+-_?="
    sn = elements $ alphanum <> "+-._?="
    limited n = do
      k <- choose (0, 210)
      vectorOf k n

instance Arbitrary StorePathHashPart where
  arbitrary =
    oneof
      [ System.Nix.StorePath.mkStorePathHashPart @MD5
        . Data.ByteString.Char8.pack <$> arbitrary
      , System.Nix.StorePath.mkStorePathHashPart @SHA1
        . Data.ByteString.Char8.pack <$> arbitrary
      , System.Nix.StorePath.mkStorePathHashPart @SHA256
        . Data.ByteString.Char8.pack <$> arbitrary
      , System.Nix.StorePath.mkStorePathHashPart @SHA512
        . Data.ByteString.Char8.pack <$> arbitrary
      ]
