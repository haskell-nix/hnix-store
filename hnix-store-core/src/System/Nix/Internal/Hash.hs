{-|
Description : Cryptographic hashes for hnix-store.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE OverloadedStrings   #-}

module System.Nix.Internal.Hash where

import qualified Crypto.Hash.MD5        as MD5
import qualified Crypto.Hash.SHA1       as SHA1
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BSC
import           Data.Bits              (xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Hashable          as DataHashable
import           Data.Kind              (Type)
import           Data.List              (foldl')
import           Data.Monoid
import           Data.Proxy             (Proxy(Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Vector            as V
import           Data.Word              (Word8)
import           GHC.TypeLits
import qualified System.Nix.Base32      as Base32

-- | A tag for different hashing algorithms
--   Also used as a type-level tag for hash digests
--   (e.g. @Digest SHA256@ is the type for a sha256 hash)
data HashAlgorithm
  = MD5
  | SHA1
  | SHA256
  | Truncated Nat HashAlgorithm

class NamedAlgo a where
  algoName :: Text

instance NamedAlgo 'MD5 where
  algoName = "md5"

instance NamedAlgo 'SHA1 where
  algoName = "sha1"

instance NamedAlgo 'SHA256 where
  algoName = "sha256"

-- | Types with kind @HashAlgorithm@ may be a @HasDigest@ instance
--   if they are able to hash bytestrings via the init/update/finalize
--   API of cryptonite
--
--   Each instance defined here simply defers to one of the underlying
--   monomorphic hashing libraries, such as `cryptohash-sha256`.
class HasDigest (a :: HashAlgorithm) where

  type AlgoCtx a :: Type

  initialize        :: AlgoCtx a
  update            :: AlgoCtx a -> BS.ByteString -> AlgoCtx a
  finalize          :: AlgoCtx a -> Digest a


-- | The cryptographic hash of of a strict bytestring, where hash
--   algorithm is chosen by the type of the digest
--   For example:
--   > let d = hash "Hello, sha-256!" :: Digest SHA256
--   or
--   > :set -XTypeApplications
--   > let d = hash @SHA256 "Hello, sha-256!"
hash :: forall a.HasDigest a => BS.ByteString -> Digest a
hash bs =
  finalize $ update @a (initialize @a) bs

-- | The cryptographic hash of a lazy bytestring. Use is the same
--   as for @hash@. This runs in constant space, but forces the
--   entire bytestring
hashLazy :: forall a.HasDigest a => BSL.ByteString -> Digest a
hashLazy bsl =
  finalize $ foldl' (update @a) (initialize @a) (BSL.toChunks bsl)

-- | Encode a Digest in the special Nix base-32 encoding.
encodeBase32 :: Digest a -> T.Text
encodeBase32 (Digest bs) = Base32.encode bs

-- | Encode a Digest in hex.
encodeBase16 :: Digest a -> T.Text
encodeBase16 (Digest bs) = T.decodeUtf8 (Base16.encode bs)


instance HasDigest MD5 where
  type AlgoCtx 'MD5 = MD5.Ctx
  initialize = MD5.init
  update = MD5.update
  finalize = Digest . MD5.finalize

instance HasDigest 'SHA1 where
  type AlgoCtx SHA1 = SHA1.Ctx
  initialize = SHA1.init
  update = SHA1.update
  finalize = Digest . SHA1.finalize

instance HasDigest 'SHA256 where
  type AlgoCtx SHA256 = SHA256.Ctx
  initialize = SHA256.init
  update = SHA256.update
  finalize = Digest . SHA256.finalize

instance (HasDigest a, KnownNat n) => HasDigest (Truncated n a) where
  type AlgoCtx (Truncated n a) = AlgoCtx a
  initialize = initialize @a
  update = update @a
  finalize = truncateDigest @n . finalize @a

-- | A raw hash digest, with a type-level tag
newtype Digest (a :: HashAlgorithm) = Digest
  { digestBytes :: BS.ByteString
    -- ^ The bytestring in a Digest is an opaque string of bytes,
    --   not some particular text encoding.
  } deriving (Show, Eq, Ord, DataHashable.Hashable)


-- | Internal function for producing the bitwise truncation of bytestrings.
--   When truncation length is greater than the length of the bytestring,
--   but less than twice the bytestring length, truncation splits the
--   bytestring into a head part (truncation length) and tail part (leftover
--   part) right-pads the leftovers with 0 to the truncation length, and
--   combines the two strings bytewise with `xor`
truncateDigest :: forall n a.(HasDigest a, KnownNat n) => Digest a -> Digest (Truncated n a)
truncateDigest (Digest c) = Digest $ BS.pack $ map truncOutputByte [0.. n-1]
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
