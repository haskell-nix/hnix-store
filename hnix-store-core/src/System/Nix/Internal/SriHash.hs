{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module System.Nix.Internal.SriHash where

import qualified "cryptohash-md5" Crypto.Hash.MD5        as MD5
import qualified "cryptohash-sha1" Crypto.Hash.SHA1       as SHA1
import qualified "cryptohash-sha256" Crypto.Hash.SHA256     as SHA256
import qualified "cryptohash-sha512" Crypto.Hash.SHA512     as SHA512
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified System.Nix.Base32      as Base32  -- Nix has own Base32 encoding
import qualified Data.ByteString.Base64 as Base64
import           Data.Bits              (xor)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Hashable          as DataHashable
import           Data.List              (find, foldl')
import           Data.Proxy             (Proxy(Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Word              (Word8)
import           GHC.TypeLits           (Nat, KnownNat, natVal)
import           Data.Coerce            (coerce)
import qualified System.Nix.Internal.Old as O

-- | Constructors to indicate the base encodings
data BaseEncoding
  = Base16
  | Base32
  -- ^ Nix has a special map of Base32 encoding
  | Base64

mkNamedDigest :: Text -> Text -> Either String O.SomeNamedDigest
mkNamedDigest name sriHash =
  let (sriName, h) = T.breakOnEnd "-" sriHash in
    if sriName == "" || sriName == (name <> "-")
    then mkDigest h
    else Left $ T.unpack $ "Sri hash method " <> sriName <> " does not match the required hash type " <> name
 where
  mkDigest :: Text -> Either String O.SomeNamedDigest
  mkDigest h =
    maybe (Left $ "Unknown hash name: " <> T.unpack name) (`decodeToSomeDigest` h) maybeFindHashTypeByName

  maybeFindHashTypeByName :: Maybe O.HashAlgorithm
  maybeFindHashTypeByName = find (\ hashType -> O.canonicalHashName hashType == name ) [O.SHA256, O.MD5, O.SHA1, O.SHA512] -- SHA256 is the most used in Nix - so it matches first

  decodeToSomeDigest :: O.HashAlgorithm -> Text -> Either String O.SomeNamedDigest
  decodeToSomeDigest O.MD5 = fmap O.SomeDigest . goDecode @'O.MD5
  decodeToSomeDigest O.SHA1 = fmap O.SomeDigest . goDecode @'O.SHA1
  decodeToSomeDigest O.SHA256 = fmap O.SomeDigest . goDecode @'O.SHA256
  decodeToSomeDigest O.SHA512 = fmap O.SomeDigest . goDecode @'O.SHA512

  goDecode :: forall a . (O.NamedAlgo a, O.ValidAlgo a) => Text -> Either String (O.Digest a)
  goDecode h =
    -- Base encoding detected by comparing the lengths of the hash in Base to the canonical length of the demanded hash type
    maybe left (`decodeBase` h) maybeFindBaseEncByLenMatch
   where
    left = Left $ T.unpack sriHash <> " is not a valid " <> T.unpack name <> " hash. Its length (" <> show (T.length h) <> ") does not match any of " <> show (canonicalLenIf <$> bases)

    maybeFindBaseEncByLenMatch = find (\ enc -> T.length h == canonicalLenIf enc) bases

    expectedHashLen = O.hashSize @a

    canonicalLenIf Base16 = 2 * expectedHashLen
    canonicalLenIf Base32 = ((8 * expectedHashLen - 1) `div` 5) + 1
    canonicalLenIf Base64 = ((4 * expectedHashLen `div` 3) + 3) `div` 4 * 4
    bases = [Base32, Base16, Base64]  -- 32 is the most used in Nix - so the first match


-- | Hash an entire (strict) 'BS.ByteString' as a single call.
--
--   For example:
--   > let d = hash "Hello, sha-256!" :: O.Digest SHA256
--   or
--   > :set -XTypeApplications
--   > let d = hash @SHA256 "Hello, sha-256!"
hash :: forall a.O.ValidAlgo a => BS.ByteString -> O.Digest a
hash bs =
  O.finalize $ O.update @a (O.initialize @a) bs

-- | Hash an entire (lazy) 'BSL.ByteString' as a single call.
--
-- Use is the same as for 'hash'.  This runs in constant space, but
-- forces the entire bytestring.
hashLazy :: forall a.O.ValidAlgo a => BSL.ByteString -> O.Digest a
hashLazy bsl =
  O.finalize $ foldl' (O.update @a) (O.initialize @a) (BSL.toChunks bsl)


-- | Take BaseEncoding type of the output -> take the Digeest as input -> encode O.Digest
encodeInBase :: BaseEncoding -> O.Digest a -> T.Text
encodeInBase Base16 = T.decodeUtf8 . Base16.encode . coerce
encodeInBase Base32 = Base32.encode . coerce
encodeInBase Base64 = T.decodeUtf8 . Base64.encode . coerce


-- | Take BaseEncoding type of the input -> take the input itself -> decodeBase into O.Digest
decodeBase :: BaseEncoding -> T.Text -> Either String (O.Digest a)
#if MIN_VERSION_base16_bytestring(1,0,0)
decodeBase Base16 = fmap O.Digest . Base16.decode . T.encodeUtf8
#else
decodeBase Base16 = lDecode  -- this tacit sugar simply makes GHC pleased with number of args
 where
  lDecode t = case Base16.decode (T.encodeUtf8 t) of
    (x, "") -> Right $ O.Digest x
    _       -> Left $ "Unable to decode base16 string" <> T.unpack t
#endif
decodeBase Base32 = fmap O.Digest . Base32.decode
decodeBase Base64 = fmap O.Digest . Base64.decode . T.encodeUtf8
