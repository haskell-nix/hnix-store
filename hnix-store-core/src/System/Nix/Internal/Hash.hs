{-|
Description : Cryptographic hashes for hnix-store.
Maintainer  : Greg Hale <imalsogreg@gmail.com>
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

module System.Nix.Internal.Hash where

import qualified Crypto.Hash.MD5       as MD5
import qualified Crypto.Hash.SHA1      as SHA1
import qualified Crypto.Hash.SHA256    as SHA256
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Bits             (xor)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Hashable         as DataHashable
import           Data.Kind             (Type)
import           Data.List             (foldl')
import           Data.Proxy            (Proxy(Proxy))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V
import           Data.Word             (Word8)
import           GHC.TypeLits

-- | A tag for different hashing algorithms
--   Also used as a type-level tag for hash digests
--   (e.g. @Digest SHA256@ is the type for a sha256 hash)
--
--  When used at the type level, `n` is `Nat`
data HashAlgorithm' n
  = MD5
  | SHA1
  | SHA256
  | Truncated n (HashAlgorithm' n)
  deriving (Eq, Show)

type HashAlgorithm = HashAlgorithm' Nat

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




-- | Convert any Digest to a base32-encoded string.
--   This is not used in producing store path hashes
printAsBase32 :: Digest a -> T.Text
printAsBase32 (Digest bs) = printHashBytes32 bs


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


-- instance DataHashable.Hashable (Digest a) where
--   hashWithSalt a (Digest bs) = DataHashable.hashWithSalt a bs
--   hashWithSalt = coerce . DataHash


-- | Internal function for encoding bytestrings into base32 according to
--  nix's convention
printHashBytes32 :: BS.ByteString -> T.Text
printHashBytes32 c = T.pack $ concatMap char32 [nChar - 1, nChar - 2 .. 0]
  where
    -- The base32 encoding is 8/5's as long as the base256 digest
    nChar = fromIntegral $ BS.length c * 8 `div` 5

    char32 :: Integer -> [Char]
    char32 i = [digits32 V.! digitInd]
      where
        byte j   = BS.index c (fromIntegral j)
        digitInd = fromIntegral $
                   sum [fromIntegral (byte j) * (256^j)
                       | j <- [0 .. BS.length c - 1]]
                   `div` (32^i)
                   `mod` 32


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

digits32 :: V.Vector Char
digits32 = V.fromList "0123456789abcdfghijklmnpqrsvwxyz"


-- | Convert type-level @HashAlgorithm@ into the value level
class AlgoVal (a :: HashAlgorithm) where
  algoVal :: HashAlgorithm' Integer

instance AlgoVal MD5 where
  algoVal = MD5

instance AlgoVal SHA1 where
  algoVal = SHA1

instance AlgoVal SHA256 where
  algoVal = SHA256

instance forall a n.(AlgoVal a, KnownNat n) => AlgoVal (Truncated n a) where
  algoVal = Truncated (natVal (Proxy @n)) (algoVal @a)
