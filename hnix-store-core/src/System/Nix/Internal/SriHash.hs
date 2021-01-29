{-# LANGUAGE ScopedTypeVariables #-}

module System.Nix.Internal.SriHash where

import qualified System.Nix.Internal.Old as O
import qualified System.Nix.Internal.Base as B

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
    maybe left (`B.decodeBase` h) maybeFindBaseEncByLenMatch
   where
    left = Left $ T.unpack sriHash <> " is not a valid " <> T.unpack name <> " hash. Its length (" <> show (T.length h) <> ") does not match any of " <> show (canonicalLenIf <$> bases)

    maybeFindBaseEncByLenMatch = find (\ enc -> T.length h == canonicalLenIf enc) bases

    expectedHashLen = O.hashSize @a

    canonicalLenIf B.Base16 = 2 * expectedHashLen
    canonicalLenIf B.Base32 = ((8 * expectedHashLen - 1) `div` 5) + 1
    canonicalLenIf B.Base64 = ((4 * expectedHashLen `div` 3) + 3) `div` 4 * 4
    bases = [B.Base32, B.Base16, B.Base64]  -- 32 is the most used in Nix - so the first match
