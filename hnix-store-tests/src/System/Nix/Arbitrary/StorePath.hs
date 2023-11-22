-- due to recent generic-arbitrary
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.StorePath where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Crypto.Hash (SHA256)
import qualified Data.ByteString.Char8
import qualified Data.Text
import System.Nix.StorePath (StoreDir(..)
  , StorePath(..)
  , StorePathName(..)
  , StorePathHashPart(..)
  )
import qualified System.Nix.StorePath

import Test.QuickCheck (Arbitrary(arbitrary), listOf, elements)

instance Arbitrary StoreDir where
  arbitrary =
    StoreDir
    . (Data.ByteString.Char8.singleton '/' <>) -- TODO(srk): nasty, see #237
    . Data.ByteString.Char8.pack <$> arbitrary

instance Arbitrary StorePath where
  arbitrary =
    liftA2 StorePath
      arbitrary
      arbitrary

instance Arbitrary StorePathName where
  arbitrary =
      either undefined id
    . System.Nix.StorePath.makeStorePathName
    . Data.Text.pack <$> ((:) <$> s1 <*> listOf sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1       = elements $ alphanum <> "+-_?="
    sn       = elements $ alphanum <> "+-._?="

instance Arbitrary StorePathHashPart where
  arbitrary =
    -- TODO(srk): other hashes
    System.Nix.StorePath.mkStorePathHashPart @SHA256
    . Data.ByteString.Char8.pack <$> arbitrary
