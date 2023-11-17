{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Description : Cryptographic hashing interface for hnix-store, on top
              of the cryptohash family of libraries.
-}

module System.Nix.Hash
  ( NamedAlgo(..)
  , SomeNamedDigest(..)
  , mkNamedDigest

  , mkStorePathHash

  , System.Nix.Base.BaseEncoding(..)
  , encodeDigestWith
  , decodeDigestWith
  ) where

import Crypto.Hash (Digest, HashAlgorithm, MD5(..), SHA1(..), SHA256(..), SHA512(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import System.Nix.Base (BaseEncoding(..))

import qualified Crypto.Hash
import qualified Data.ByteArray
import qualified Data.Text
import qualified System.Nix.Base
import qualified System.Nix.Hash.Truncation

-- | A 'HashAlgorithm' with a canonical name, for serialization
-- purposes (e.g. SRI hashes)
class HashAlgorithm a => NamedAlgo a where
  algoName :: Text

instance NamedAlgo MD5 where
  algoName = "md5"

instance NamedAlgo SHA1 where
  algoName = "sha1"

instance NamedAlgo SHA256 where
  algoName = "sha256"

instance NamedAlgo SHA512 where
  algoName = "sha512"

-- | A digest whose 'NamedAlgo' is not known at compile time.
data SomeNamedDigest = forall a . NamedAlgo a => SomeDigest (Digest a)

instance Show SomeNamedDigest where
  show sd = case sd of
    SomeDigest (digest :: Digest hashType) ->
      Data.Text.unpack $ "SomeDigest"
      <> " "
      <> algoName @hashType
      <> ":"
      <> encodeDigestWith NixBase32 digest

instance Eq SomeNamedDigest where
  (==) (SomeDigest (a :: Digest aType))
       (SomeDigest (b :: Digest bType))
    = algoName @aType == algoName @bType
      && encodeDigestWith NixBase32 a == encodeDigestWith NixBase32 b

instance Ord SomeNamedDigest where
  (<=) (SomeDigest (a :: Digest aType))
       (SomeDigest (b :: Digest bType))
    = algoName @aType <= algoName @bType
      && encodeDigestWith NixBase32 a <= encodeDigestWith NixBase32 b

mkNamedDigest :: Text -> Text -> Either String SomeNamedDigest
mkNamedDigest name sriHash =
  let (sriName, h) = Data.Text.breakOnEnd "-" sriHash in
    if sriName == "" || sriName == name <> "-"
    then mkDigest h
    else
      Left
      $ Data.Text.unpack
      $ "Sri hash method"
      <> " "
      <> sriName
      <> " "
      <> "does not match the required hash type"
      <> " "
      <> name
 where
  mkDigest h = case name of
    "md5"    -> SomeDigest <$> decodeGo MD5    h
    "sha1"   -> SomeDigest <$> decodeGo SHA1   h
    "sha256" -> SomeDigest <$> decodeGo SHA256 h
    "sha512" -> SomeDigest <$> decodeGo SHA512 h
    _        -> Left $ "Unknown hash name: " <> Data.Text.unpack name
  decodeGo :: forall a . NamedAlgo a => a -> Text -> Either String (Digest a)
  decodeGo a h
    | size == base16Len = decodeDigestWith Base16 h
    | size == base32Len = decodeDigestWith NixBase32 h
    | size == base64Len = decodeDigestWith Base64 h
    | otherwise =
        Left
        $ Data.Text.unpack
        $ sriHash
        <> " "
        <> "is not a valid"
        <> " "
        <> name
        <> " "
        <> "hash. Its length ("
        <> Data.Text.pack (show size)
        <> ") does not match any of"
        <> " "
        <> Data.Text.pack (show [base16Len, base32Len, base64Len])
   where
    size = Data.Text.length h
    hsize = Crypto.Hash.hashDigestSize a
    base16Len = hsize * 2
    base32Len = ((hsize * 8 - 1) `div` 5) + 1;
    base64Len = ((4 * hsize `div` 3) + 3) `div` 4 * 4;

mkStorePathHash
  :: forall a
   . HashAlgorithm a
  => ByteString
  -> ByteString
mkStorePathHash bs =
  System.Nix.Hash.Truncation.truncateInNixWay 20
  $ Data.ByteArray.convert
  $ Crypto.Hash.hash @ByteString @a bs

-- | Take BaseEncoding type of the output -> take the Digeest as input -> encode Digest
encodeDigestWith :: BaseEncoding -> Digest a -> Text
encodeDigestWith b = System.Nix.Base.encodeWith b . Data.ByteArray.convert

-- | Take BaseEncoding type of the input -> take the input itself -> decodeBase into Digest
decodeDigestWith
  :: HashAlgorithm a
  => BaseEncoding
  -> Text
  -> Either String (Digest a)
decodeDigestWith b x =
  do
    bs <- System.Nix.Base.decodeWith b x
    let
      toEither =
        maybeToRight
          ("Cryptonite was not able to convert '(ByteString -> Digest a)' for: '" <> show bs <>"'.")
    (toEither . Crypto.Hash.digestFromByteString) bs
  where
    -- To not depend on @extra@
    maybeToRight :: b -> Maybe a -> Either b a
    maybeToRight _ (Just r) = pure r
    maybeToRight y Nothing  = Left y
