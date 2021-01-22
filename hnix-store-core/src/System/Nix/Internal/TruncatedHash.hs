
module System.Nix.Internal.TruncatedHash where

-- data TruncatedHash = Truncated HashAlgorithm Integer
-- ^ The hash algorithm obtained by truncating the result of the
-- input 'HashAlgorithm' to the given number of bytes. See
-- 'truncateDigest' for a description of the truncation algorithm.

-- TODO: HNix really uses 1 type/length of truncation.
-- | Bytewise truncation of a 'Digest'.
--
-- When truncation length is greater than the length of the bytestring
-- but less than twice the bytestring length, truncation splits the
-- bytestring into a head part (truncation length) and tail part
-- (leftover part), right-pads the leftovers with 0 to the truncation
-- length, and combines the two strings bytewise with 'xor'.
-- truncateDigest
--   :: forall n a.(KnownNat n) => Digest a -> Digest ('Truncated n a)
-- truncateDigest (Digest c) =
--     Digest $ BS.pack $ map truncOutputByte [0.. n-1]
--   where
--     n = fromIntegral $ natVal (Proxy @n)

--     truncOutputByte :: Int -> Word8
--     truncOutputByte i = foldl' (aux i) 0 [0 .. BS.length c - 1]

--     inputByte :: Int -> Word8
--     inputByte j = BS.index c (fromIntegral j)

--     aux :: Int -> Word8 -> Int -> Word8
--     aux i x j = if j `mod` fromIntegral n == fromIntegral i
--                 then xor x (inputByte $ fromIntegral j)
--                 else x
