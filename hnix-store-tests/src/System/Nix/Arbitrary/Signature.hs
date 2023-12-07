-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Nix.Arbitrary.Signature where

import qualified Crypto.PubKey.Ed25519
import Crypto.Random (drgNewTest, withDRG)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck

import System.Nix.Signature

instance Arbitrary Crypto.PubKey.Ed25519.Signature where
  arbitrary = do
    seeds <- (,,,,) <$> arbitraryBoundedRandom <*> arbitraryBoundedRandom <*> arbitraryBoundedRandom <*> arbitraryBoundedRandom <*> arbitraryBoundedRandom
    let drg = drgNewTest seeds
        (secretKey, _) = withDRG drg Crypto.PubKey.Ed25519.generateSecretKey
        publicKey = Crypto.PubKey.Ed25519.toPublic secretKey
        msg :: BS.ByteString = "msg"
    pure $ Crypto.PubKey.Ed25519.sign secretKey publicKey msg

deriving via GenericArbitrary Signature
  instance Arbitrary Signature

instance Arbitrary NarSignature where
  arbitrary = do
    name <- Text.pack . getPrintableString <$> suchThat arbitrary (\(PrintableString str) -> validName str)
    NarSignature name <$> arbitrary

validName :: String -> Bool
validName txt = not (null txt) && not (elem ':' txt)

