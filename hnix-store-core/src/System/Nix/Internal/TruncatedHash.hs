{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExistentialQuantification #-}


module System.Nix.Internal.TruncatedHash where


import qualified Data.ByteString        as ByteString
import           Data.Word              (Word8)
import Data.Foldable (foldl')
import           Data.Bits              (xor)
import System.Nix.Internal.Old          as Old
import Data.Bool (bool)
import GHC.TypeLits (Nat, KnownNat, natVal)
import           Data.Proxy             (Proxy(Proxy))

-- TODO: HNix really uses 1 type/length of truncation.

-- data TruncatedHash = Truncated Nat Old.HashAlgorithm

-- | Reuses the underlying 'ValidAlgo' instance, but does a
-- 'truncateDigest' at the end.
instance (Old.ValidAlgo a, KnownNat n) => Old.ValidAlgo ('Old.Truncated n a) where
  type AlgoCtx ('Old.Truncated n a) = AlgoCtx a
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
  :: forall n a.(KnownNat n) => Old.Digest a -> Old.Digest ('Old.Truncated n a)
truncateDigest (Old.Digest c) =
    Old.Digest $ ByteString.pack $ map truncOutputByte [0.. n-1]
  where
    n = fromIntegral $ natVal (Proxy @n)

    truncOutputByte :: Int -> Word8
    truncOutputByte i = foldl' (aux i) 0 [0 .. ByteString.length c - 1]

    inputByte :: Int -> Word8
    inputByte j = ByteString.index c (fromIntegral j)

    aux :: Int -> Word8 -> Int -> Word8
    aux i x j = if j `mod` fromIntegral n == fromIntegral i
                then xor x (inputByte $ fromIntegral j)
                else x
