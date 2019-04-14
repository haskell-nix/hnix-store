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
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
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
  | Truncated Nat HashAlgorithm
    -- ^ The hash algorithm obtained by truncating the result of the
    -- input 'HashAlgorithm' to the given number of bytes. See
    -- 'truncateDigest' for a description of the truncation algorithm.

-- | The result of running a 'HashAlgorithm'.
newtype Digest (a :: HashAlgorithm) =
  Digest BS.ByteString deriving (Show, Eq, Ord, DataHashable.Hashable)

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
class NamedAlgo (a :: HashAlgorithm) where
  algoName :: Text

instance NamedAlgo 'MD5 where
  algoName = "md5"

instance NamedAlgo 'SHA1 where
  algoName = "sha1"

instance NamedAlgo 'SHA256 where
  algoName = "sha256"

-- | A digest whose 'NamedAlgo' is not known at compile time.
data SomeNamedDigest = forall a . NamedAlgo a => SomeDigest (Digest a)

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

-- | Hash file
hashFile :: forall a.ValidAlgo a => FilePath -> IO (Digest a)
hashFile fp = hashLazy <$> BSL.readFile fp

digestText32 :: forall a. (NamedAlgo a, ValidAlgo a) => Digest a -> T.Text
digestText32 d = algoName @a <> ":" <> encodeBase32 d

digestText16 :: forall a. NamedAlgo a => Digest a -> T.Text
digestText16 d = algoName @a <> ":" <> encodeBase16 d

-- | Encode a 'Digest' in the special Nix base-32 encoding.
encodeBase32 :: Digest a -> T.Text
encodeBase32 (Digest bs) = Base32.encode bs

-- | Encode a 'Digest' in hex.
encodeBase16 :: Digest a -> T.Text
encodeBase16 (Digest bs) = T.decodeUtf8 (Base16.encode bs)

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
