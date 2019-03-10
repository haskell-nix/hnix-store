{-|
Description : Cryptographic hashes for hnix-store.
Maintainer  : Greg Hale <imalsogreg@gmail.com>
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module System.Nix.Internal.Hash where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Bits (xor)
import           Data.Kind (Type)
import           Data.List (foldl')
import           Data.Monoid
import           Data.Proxy (Proxy(Proxy))
import           Data.Text (Text)
import           Data.Word (Word8)
import           Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import           GHC.TypeLits
import           Numeric.Natural

-- | A tag for different hashing algorithms
--   Also used as a type-level tag for hash digests
--   (e.g. @Digest SHA256@ is the type for a sha256 hash)
--
--  When used at the type level, `n` is `Nat`
data HashAlgorithm
  = MD5
  | SHA1
  | SHA256
  deriving (Eq, Ord, Show)

data HashForm' n
  = Plain HashAlgorithm
  | Truncated n HashAlgorithm
  deriving (Eq, Ord, Show)

type HashForm = HashForm' Nat

class HasDigest (Plain a) => NamedAlgorithm (a :: HashAlgorithm) where
  algorithmName :: forall a. Text

instance NamedAlgorithm 'MD5 where
  algorithmName = "md5"

instance NamedAlgorithm 'SHA1 where
  algorithmName = "sha1"

instance NamedAlgorithm 'SHA256 where
  algorithmName = "sha256"

-- | Types with kind @HashAlgorithm@ may be a @HasDigest@ instance
--   if they are able to hash bytestrings via the init/update/finalize
--   API of cryptonite
--
--   Each instance defined here simply defers to one of the underlying
--   monomorphic hashing libraries, such as `cryptohash-sha256`.
class HasDigest (a :: HashForm) where

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

digestText32 :: forall a. NamedAlgorithm a => Digest ('Plain a) -> T.Text
digestText32 d = algorithmName @a <> ":" <> printAsBase32 d

digestText16 :: forall a. NamedAlgorithm a => Digest ('Plain a) -> T.Text
digestText16 (Digest bs) = algorithmName @a <> ":" <> T.decodeUtf8 (Base16.encode bs)

-- | Convert any Digest to a base32-encoded string.
--   This is not used in producing store path hashes
printAsBase32 :: Digest a -> T.Text
printAsBase32 (Digest bs) = printHashBytes32 bs

instance HasDigest ('Plain 'MD5) where
  type AlgoCtx (Plain 'MD5) = MD5.Ctx
  initialize = MD5.init
  update = MD5.update
  finalize = Digest . MD5.finalize

instance HasDigest ('Plain 'SHA1) where
  type AlgoCtx (Plain SHA1) = SHA1.Ctx
  initialize = SHA1.init
  update = SHA1.update
  finalize = Digest . SHA1.finalize

instance HasDigest ('Plain 'SHA256) where
  type AlgoCtx (Plain SHA256) = SHA256.Ctx
  initialize = SHA256.init
  update = SHA256.update
  finalize = Digest . SHA256.finalize

instance (HasDigest ('Plain a), KnownNat n) => HasDigest ('Truncated n a) where
  type AlgoCtx ('Truncated n a) = AlgoCtx ('Plain a)
  initialize = initialize @('Plain a)
  update = update @('Plain a)
  finalize = truncateDigest @n @a . finalize @('Plain a)

-- | A raw hash digest, with a type-level tag
newtype Digest (a :: HashForm) = Digest
  { digestBytes :: BS.ByteString
    -- ^ The bytestring in a Digest is an opaque string of bytes,
    --   not some particular text encoding.
  } deriving (Show, Eq, Ord, Hashable)

-- | A digest from a named hash algorithm.
data AnyDigest =
  forall a . HasDigest a => AnyDigest (Digest a)

--instance Show AnyDigest
--instance Eq AnyDigest
--instance Ord AnyDigest
--instance Hashable AnyDigest where
--  hashWithSalt salt (AnyDigest bs) = hashWithSalt salt bs

-- instance DataHashable.Hashable (Digest a) where
--   hashWithSalt a (Digest bs) = DataHashable.hashWithSalt a bs
--   hashWithSalt = coerce . DataHash


-- | Internal function for encoding bytestrings into base32 according to
--  nix's convention
printHashBytes32 :: BS.ByteString -> T.Text
printHashBytes32 c = T.pack $ concatMap char32 [nChar - 1, nChar - 2 .. 0]
  where
    -- The base32 encoding is 8/5's as long as the base256 digest.  This `+ 1`
    -- `- 1` business is a bit odd, but has always been used in C++ since the
    -- base32 truncation was added in was first added in
    -- d58a11e019813902b6c4547ca61a127938b2cc20.
    nChar = fromIntegral $ ((BS.length c * 8 - 1) `div` 5) + 1

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
truncateDigest
  :: forall n a
  . (HasDigest ('Plain a), KnownNat n)
  => Digest ('Plain a)
  -> Digest (Truncated n a)
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
  algoVal :: forall a. HashAlgorithm

instance AlgoVal MD5 where
  algoVal = MD5

instance AlgoVal SHA1 where
  algoVal = SHA1

instance AlgoVal SHA256 where
  algoVal = SHA256

class FormVal (a :: HashForm) where
  formVal :: HashForm' Natural

instance forall a. AlgoVal a => FormVal (Plain a) where
  formVal = Plain $ algoVal @a

instance forall a n. (AlgoVal a, KnownNat n) => FormVal (Truncated n a) where
  formVal = Truncated (fromIntegral $ natVal (Proxy @n)) (algoVal @a)
