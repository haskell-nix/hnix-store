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
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module System.Nix.Internal.Uncycle where


import qualified "cryptohash-md5" Crypto.Hash.MD5        as MD5
import qualified "cryptohash-sha1" Crypto.Hash.SHA1       as SHA1
import qualified "cryptohash-sha256" Crypto.Hash.SHA256     as SHA256
import qualified "cryptohash-sha512" Crypto.Hash.SHA512     as SHA512
import qualified Data.ByteString        as BS
import           Data.Bits              (xor)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Hashable          as DataHashable
import           Data.List              (foldl')
import           Data.Proxy             (Proxy(Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Word              (Word8)
import           GHC.TypeLits           (Nat, KnownNat, natVal)
import qualified System.Nix.Internal.Base as B
import qualified System.Nix.Internal.Old as O


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

class HashProperties a
 where
  canonicalHashName :: a -> Text
  canonicalHashLen :: a -> Int

instance HashProperties HashAlgorithm
 where
  canonicalHashName SHA256 = "sha256"  -- SHA256 is the most used in Nix - so it matches first
  canonicalHashName MD5    = "md5"
  canonicalHashName SHA1   = "sha1"
  canonicalHashName SHA512 = "sha512"
  canonicalHashLen  SHA256 = 32
  canonicalHashLen  MD5    = 16
  canonicalHashLen  SHA1   = 20
  canonicalHashLen  SHA512 = 64

-- | The result of running a 'HashAlgorithm'.
newtype Digest (a :: HashAlgorithm) =
  Digest BS.ByteString deriving (Eq, Ord, DataHashable.Hashable)

instance Show (Digest a) where
  show = ("Digest " <>) . show . encodeInBase Base32

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
  show (SomeDigest (digest :: Digest hashType)) = T.unpack $ "SomeDigest " <> algoName @hashType <> ":" <> encodeInBase Base32 digest


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
