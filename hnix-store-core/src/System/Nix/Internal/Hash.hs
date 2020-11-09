{-|
Description : Cryptographic hashing interface for hnix-store, on top
              of the cryptohash family of libraries.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExistentialQuantification #-}

module System.Nix.Internal.Hash where

import qualified Crypto.Hash.MD5        as MD5
import qualified Crypto.Hash.SHA1       as SHA1
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Crypto.Hash.SHA512     as SHA512
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import           Data.Bits              (xor)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Hashable          as DataHashable
import           Data.List              (foldl')
import           Data.Proxy             (Proxy(Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Word              (Word8)
import           GHC.TypeLits           (Nat, KnownNat, natVal)
import qualified System.Nix.Base32      as Base32

-- | The universe of supported hash algorithms.
--
-- Currently only intended for use at the type level.
data HashAlgorithm
  = MD5
  | SHA1
  | SHA256
  | SHA512
  | Truncated Nat HashAlgorithm
    -- ^ The hash algorithm obtained by truncating the result of the
    -- input 'HashAlgorithm' to the given number of bytes. See
    -- 'truncateDigest' for a description of the truncation algorithm.

-- | The result of running a 'HashAlgorithm'.
newtype Digest (a :: HashAlgorithm) =
  Digest BS.ByteString deriving (Eq, Ord, DataHashable.Hashable)

instance Show (Digest a) where
  show = ("Digest " ++) . show . encodeBase32

-- | The primitive interface for incremental hashing for a given
-- 'HashAlgorithm'. Every 'HashAlgorithm' should have an instance.
class ValidAlgo (a :: HashAlgorithm) where
  -- | The incremental state for constructing a hash.
  type AlgoCtx a

  -- | Start building a new hash.
  initialize        :: AlgoCtx a
  -- | Append a 'BS.ByteString' to the overall contents to be hashed.
  update            :: AlgoCtx a -> BS.ByteString -> AlgoCtx a
  -- | Finish hashing and generate the output.
  finalize          :: AlgoCtx a -> Digest a

-- | A 'HashAlgorithm' with a canonical name, for serialization
-- purposes (e.g. SRI hashes)
class ValidAlgo a => NamedAlgo (a :: HashAlgorithm) where
  algoName :: Text
  hashSize :: Int

instance NamedAlgo 'MD5 where
  algoName = "md5"
  hashSize = 16

instance NamedAlgo 'SHA1 where
  algoName = "sha1"
  hashSize = 20

instance NamedAlgo 'SHA256 where
  algoName = "sha256"
  hashSize = 32

instance NamedAlgo 'SHA512 where
  algoName = "sha512"
  hashSize = 64

-- | A digest whose 'NamedAlgo' is not known at compile time.
data SomeNamedDigest = forall a . NamedAlgo a => SomeDigest (Digest a)

instance Show SomeNamedDigest where
  show sd = case sd of
    SomeDigest (digest :: Digest hashType) -> T.unpack $ "SomeDigest " <> algoName @hashType <> ":" <> encodeBase32 digest

mkNamedDigest :: Text -> Text -> Either String SomeNamedDigest
mkNamedDigest name sriHash =
  let (sriName, hash) = T.breakOnEnd "-" sriHash in
    if sriName == "" || sriName == (name <> "-")
    then mkDigest name hash
    else Left $ T.unpack $ "Sri hash method " <> sriName <> " does not match the required hash type " <> name
 where
  mkDigest name hash = case name of
    "md5"    -> SomeDigest <$> decode @'MD5    hash
    "sha1"   -> SomeDigest <$> decode @'SHA1   hash
    "sha256" -> SomeDigest <$> decode @'SHA256 hash
    "sha512" -> SomeDigest <$> decode @'SHA512 hash
    _        -> Left $ "Unknown hash name: " ++ T.unpack name
  decode :: forall a . (NamedAlgo a, ValidAlgo a) => Text -> Either String (Digest a)
  decode hash
    | size == base16Len = decodeBase16 hash
    | size == base32Len = decodeBase32 hash
    | size == base64Len = decodeBase64 hash
    | otherwise = Left $ T.unpack sriHash ++ " is not a valid " ++ T.unpack name ++ " hash. Its length (" ++ show size ++ ") does not match any of " ++ show [base16Len, base32Len, base64Len]
   where
    size = T.length hash
    hsize = hashSize @a
    base16Len = hsize * 2
    base32Len = ((hsize * 8 - 1) `div` 5) + 1;
    base64Len = ((4 * hsize `div` 3) + 3) `div` 4 * 4;


-- | Hash an entire (strict) 'BS.ByteString' as a single call.
--
--   For example:
--   > let d = hash "Hello, sha-256!" :: Digest SHA256
--   or
--   > :set -XTypeApplications
--   > let d = hash @SHA256 "Hello, sha-256!"
hash :: forall a.ValidAlgo a => BS.ByteString -> Digest a
hash bs =
  finalize $ update @a (initialize @a) bs

-- | Hash an entire (lazy) 'BSL.ByteString' as a single call.
--
-- Use is the same as for 'hash'.  This runs in constant space, but
-- forces the entire bytestring.
hashLazy :: forall a.ValidAlgo a => BSL.ByteString -> Digest a
hashLazy bsl =
  finalize $ foldl' (update @a) (initialize @a) (BSL.toChunks bsl)

-- | Encode a 'Digest' in the special Nix base-32 encoding.
encodeBase32 :: Digest a -> T.Text
encodeBase32 (Digest bs) = Base32.encode bs

-- | Decode a 'Digest' in the special Nix base-32 encoding.
decodeBase32 :: T.Text -> Either String (Digest a)
decodeBase32 t = Digest <$> Base32.decode t

-- | Encode a 'Digest' in hex.
encodeBase16 :: Digest a -> T.Text
encodeBase16 (Digest bs) = T.decodeUtf8 (Base16.encode bs)

-- | Decode a 'Digest' in hex
decodeBase16 :: T.Text -> Either String (Digest a)
decodeBase16 t = case Base16.decode (T.encodeUtf8 t) of
  (x, "") -> Right $ Digest x
  _       -> Left $ "Unable to decode base16 string " ++ T.unpack t

-- | Encode a 'Digest' in hex.
encodeBase64 :: Digest a -> T.Text
encodeBase64 (Digest bs) = T.decodeUtf8 (Base64.encode bs)

-- | Decode a 'Digest' in hex
decodeBase64 :: T.Text -> Either String (Digest a)
decodeBase64 t = case Base64.decode (T.encodeUtf8 t) of
  Right x -> Right $ Digest x
  Left  e -> Left $ "Unable to decode base64 string " ++ T.unpack t ++ ": " ++ e

-- | Uses "Crypto.Hash.MD5" from cryptohash-md5.
instance ValidAlgo 'MD5 where
  type AlgoCtx 'MD5 = MD5.Ctx
  initialize = MD5.init
  update = MD5.update
  finalize = Digest . MD5.finalize

-- | Uses "Crypto.Hash.SHA1" from cryptohash-sha1.
instance ValidAlgo 'SHA1 where
  type AlgoCtx 'SHA1 = SHA1.Ctx
  initialize = SHA1.init
  update = SHA1.update
  finalize = Digest . SHA1.finalize

-- | Uses "Crypto.Hash.SHA256" from cryptohash-sha256.
instance ValidAlgo 'SHA256 where
  type AlgoCtx 'SHA256 = SHA256.Ctx
  initialize = SHA256.init
  update = SHA256.update
  finalize = Digest . SHA256.finalize

-- | Uses "Crypto.Hash.SHA512" from cryptohash-sha512.
instance ValidAlgo 'SHA512 where
  type AlgoCtx 'SHA512 = SHA512.Ctx
  initialize = SHA512.init
  update = SHA512.update
  finalize = Digest . SHA512.finalize

-- | Reuses the underlying 'ValidAlgo' instance, but does a
-- 'truncateDigest' at the end.
instance (ValidAlgo a, KnownNat n) => ValidAlgo ('Truncated n a) where
  type AlgoCtx ('Truncated n a) = AlgoCtx a
  initialize = initialize @a
  update = update @a
  finalize = truncateDigest @n . finalize @a

-- | Bytewise truncation of a 'Digest'.
--
-- When truncation length is greater than the length of the bytestring
-- but less than twice the bytestring length, truncation splits the
-- bytestring into a head part (truncation length) and tail part
-- (leftover part), right-pads the leftovers with 0 to the truncation
-- length, and combines the two strings bytewise with 'xor'.
truncateDigest
  :: forall n a.(KnownNat n) => Digest a -> Digest ('Truncated n a)
truncateDigest (Digest c) =
    Digest $ BS.pack $ map truncOutputByte [0.. n-1]
  where
    n = fromIntegral $ natVal (Proxy @n)

    truncOutputByte :: Int -> Word8
    truncOutputByte i = foldl' (aux i) 0 [0 .. BS.length c - 1]

    inputByte :: Int -> Word8
    inputByte j = BS.index c (fromIntegral j)

    aux :: Int -> Word8 -> Int -> Word8
    aux i x j = if j `mod` fromIntegral n == fromIntegral i
                then xor x (inputByte $ fromIntegral j)
                else x
