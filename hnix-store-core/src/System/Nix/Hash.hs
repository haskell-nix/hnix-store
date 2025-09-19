{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Description : Cryptographic hashing interface for hnix-store, on top
              of the cryptohash family of libraries.
-}

module System.Nix.Hash
  ( HashAlgo(..)
  , NamedAlgo(..)
  , algoToText
  , textToAlgo
  , mkNamedDigest

  , mkStorePathHash

  , System.Nix.Base.BaseEncoding(..)
  , encodeDigestWith
  , decodeDigestWith

  , algoDigestBuilder
  , digestBuilder
  ) where

import Control.DeepSeq (NFData(..))
import Crypto.Hash (Digest, HashAlgorithm, MD5(..), SHA1(..), SHA256(..), SHA512(..))
import Data.ByteString (ByteString)
import Data.Constraint.Extras (Has(has))
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum((:=>)))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.DeepSeq (GNFData(..))
import Data.GADT.Show.TH (deriveGShow)
import Data.Kind (Type)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import System.Nix.Base (BaseEncoding(..))

import Crypto.Hash qualified
import Data.ByteArray qualified
import Data.Text qualified
import Data.Text.Lazy.Builder qualified
import System.Nix.Base qualified
import System.Nix.Hash.Truncation qualified

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

data HashAlgo :: Type -> Type where
  HashAlgo_MD5 :: HashAlgo MD5
  HashAlgo_SHA1 :: HashAlgo SHA1
  HashAlgo_SHA256 :: HashAlgo SHA256
  HashAlgo_SHA512 :: HashAlgo SHA512

deriveGEq ''HashAlgo
deriveGCompare ''HashAlgo
deriveGShow ''HashAlgo
deriveArgDict ''HashAlgo

instance NFData (HashAlgo a) where
  rnf = \case
    HashAlgo_MD5 -> ()
    HashAlgo_SHA1 -> ()
    HashAlgo_SHA256 -> ()
    HashAlgo_SHA512 -> ()

instance GNFData HashAlgo where
  grnf = rnf

algoToText :: forall t. HashAlgo t -> Text
algoToText x = has @NamedAlgo x (algoName @t)

hashAlgoValue :: HashAlgo a -> a
hashAlgoValue = \case
  HashAlgo_MD5 -> MD5
  HashAlgo_SHA1 -> SHA1
  HashAlgo_SHA256 -> SHA256
  HashAlgo_SHA512 -> SHA512

textToAlgo :: Text -> Either String (Some HashAlgo)
textToAlgo = \case
    "md5"    -> Right $ Some HashAlgo_MD5
    "sha1"   -> Right $ Some HashAlgo_SHA1
    "sha256" -> Right $ Some HashAlgo_SHA256
    "sha512" -> Right $ Some HashAlgo_SHA512
    name     -> Left $ "Unknown hash name: " <> Data.Text.unpack name

-- | Make @DSum HashAlgo Digest@ based on provided SRI hash name
-- and its encoded form
mkNamedDigest
  :: Text -- ^ SRI name
  -> Text -- ^ base encoded hash
  -> Either String (DSum HashAlgo Digest)
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
  mkDigest h =
    textToAlgo name
    >>= \(Some a) -> has @HashAlgorithm a $ fmap (a :=>) $ decodeGo a h
  decodeGo :: HashAlgorithm a => HashAlgo a -> Text -> Either String (Digest a)
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
    hsize = Crypto.Hash.hashDigestSize (hashAlgoValue a)
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
          ("Crypton was not able to convert '(ByteString -> Digest a)' for: '" <> show bs <>"'.")
    (toEither . Crypto.Hash.digestFromByteString) bs
  where
    -- To not depend on @extra@
    maybeToRight :: b -> Maybe a -> Either b a
    maybeToRight _ (Just r) = pure r
    maybeToRight y Nothing  = Left y

-- | Builder for @Digest@s
digestBuilder :: forall hashAlgo . (NamedAlgo hashAlgo) => Digest hashAlgo -> Builder
digestBuilder digest =
  Data.Text.Lazy.Builder.fromText (System.Nix.Hash.algoName @hashAlgo)
  <> ":"
  <> Data.Text.Lazy.Builder.fromText
      (System.Nix.Hash.encodeDigestWith NixBase32 digest)

-- | Builder for @DSum HashAlgo Digest@s
algoDigestBuilder :: DSum HashAlgo Digest -> Builder
algoDigestBuilder (a :=> d) = has @NamedAlgo a $ digestBuilder d
