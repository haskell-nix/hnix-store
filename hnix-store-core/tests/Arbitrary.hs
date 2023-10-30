{-# language DataKinds            #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Arbitrary where

import qualified Data.ByteString.Char8         as BSC

import           Test.Tasty.QuickCheck

import           System.Nix.Internal.StorePath
import           Crypto.Hash                        ( SHA256
                                                    , Digest
                                                    , hash
                                                    )

genSafeChar :: Gen Char
genSafeChar = choose ('\1', '\127') -- ASCII without \NUL

nonEmptyString :: Gen String
nonEmptyString = listOf1 genSafeChar

dir :: Gen String
dir = ('/':) <$> listOf1 (elements $ '/':['a'..'z'])

instance Arbitrary StorePathName where
  arbitrary = StorePathName . toText <$> ((:) <$> s1 <*> listOf sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1       = elements $ alphanum <> "+-_?="
    sn       = elements $ alphanum <> "+-._?="

instance Arbitrary StorePathHashPart where
  arbitrary = mkStorePathHashPart . BSC.pack <$> arbitrary

instance Arbitrary (Digest SHA256) where
  arbitrary = hash . BSC.pack <$> arbitrary

instance Arbitrary StoreDir where
  arbitrary = StoreDir . ("/" <>) . BSC.pack <$> arbitrary

newtype NixLike = NixLike {getNixLike :: StorePath}
 deriving (Eq, Ord, Show)

instance Arbitrary NixLike where
  arbitrary =
    NixLike <$>
      liftA2 StorePath
        arbitraryTruncatedDigest
        arbitrary
   where
    -- 160-bit hash, 20 bytes, 32 chars in base32
    arbitraryTruncatedDigest = coerce . BSC.pack <$> replicateM 20 genSafeChar

instance Arbitrary StorePath where
  arbitrary =
    liftA2 StorePath
      arbitrary
      arbitrary
