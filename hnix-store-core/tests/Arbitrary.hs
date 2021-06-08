{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Arbitrary where

import           Control.Monad                  ( replicateM )
import qualified Data.ByteString.Char8         as BSC
import qualified Data.Text                     as T

import           Test.Tasty.QuickCheck

import           System.Nix.Hash
import           System.Nix.StorePath
import           System.Nix.Internal.StorePath
import           Control.Applicative                ( liftA3 )
import           Data.Coerce                        ( coerce )

genSafeChar :: Gen Char
genSafeChar = choose ('\1', '\127') -- ASCII without \NUL

nonEmptyString :: Gen String
nonEmptyString = listOf1 genSafeChar

dir :: Gen String
dir = ('/':) <$> listOf1 (elements $ '/':['a'..'z'])

instance Arbitrary StorePathName where
  arbitrary = StorePathName . T.pack <$> ((:) <$> s1 <*> listOf sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1       = elements $ alphanum <> "+-_?="
    sn       = elements $ alphanum <> "+-._?="

instance Arbitrary StorePathHashPart where
  arbitrary = mkStorePathHashPart . BSC.pack <$> arbitrary

instance Arbitrary (Digest 'SHA256) where
  arbitrary = hash . BSC.pack <$> arbitrary

newtype NixLike = NixLike {getNixLike :: StorePath}
 deriving (Eq, Ord, Show)

instance Arbitrary NixLike where
  arbitrary =
    NixLike <$>
      (liftA3 StorePath
        arbitraryTruncatedDigest
        arbitrary
        (pure "/nix/store")
      )
   where
    -- 160-bit hash, 20 bytes, 32 chars in base32
    arbitraryTruncatedDigest = coerce . BSC.pack <$> replicateM 20 genSafeChar

instance Arbitrary StorePath where
  arbitrary =
    liftA3 StorePath
      arbitrary
      arbitrary
      dir
