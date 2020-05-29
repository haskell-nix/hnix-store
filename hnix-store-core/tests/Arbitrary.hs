{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}

module Arbitrary where

import           Control.Monad               (replicateM)
import qualified Data.ByteString.Char8       as BSC
import qualified Data.Text                   as T

import           Test.Tasty.QuickCheck

import           System.Nix.Hash
import           System.Nix.Internal.Hash
import           System.Nix.StorePath
import           System.Nix.Internal.StorePath

genSafeChar :: Gen Char
genSafeChar = choose ('\1', '\127') -- ASCII without \NUL

nonEmptyString :: Gen String
nonEmptyString = listOf1 genSafeChar

dir = ('/':) <$> (listOf1 $ elements $ ('/':['a'..'z']))

instance Arbitrary StorePathName where
  arbitrary = StorePathName . T.pack
    <$> ((:) <$> s1 <*> listOf sn)
    where
      alphanum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      s1 = elements $ alphanum ++ "+-_?="
      sn = elements $ alphanum ++ "+-._?="

instance Arbitrary (Digest StorePathHashAlgo) where
  arbitrary = hash . BSC.pack <$> arbitrary

instance Arbitrary (Digest SHA256) where
  arbitrary = hash . BSC.pack <$> arbitrary

newtype NixLike = NixLike {getNixLike :: StorePath}
 deriving (Eq, Ord, Show)

instance Arbitrary (NixLike) where
  arbitrary = NixLike <$>
    (StorePath
    <$> arbitraryTruncatedDigest
    <*> arbitrary
    <*> pure "/nix/store")
    where
      -- 160-bit hash, 20 bytes, 32 chars in base32
      arbitraryTruncatedDigest = Digest . BSC.pack
        <$> replicateM 20 genSafeChar

instance Arbitrary StorePath where
  arbitrary = StorePath
           <$> arbitrary
           <*> arbitrary
           <*> dir
