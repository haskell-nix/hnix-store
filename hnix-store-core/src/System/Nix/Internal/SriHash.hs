{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module System.Nix.Internal.SriHash where

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified System.Nix.Internal.Old as Old
import qualified System.Nix.Internal.Base as Base
import Data.Foldable (find)

mkNamedDigest :: Text -> Text -> Either String (Old.Digest a)
mkNamedDigest name sriHash =
  let (sriName, h) = Text.breakOnEnd "-" sriHash in
    if sriName == "" || sriName == (name <> "-")
    then mkDigest h
    else Left $ Text.unpack $ "Sri hash method " <> sriName <> " does not match the required hash type " <> name
 where
  mkDigest :: Text -> Either String (Old.Digest a)
  mkDigest h =
    maybe (Left $ "Unknown hash name: " <> Text.unpack name) (`decodeToSomeDigest` h) maybeFindHashTypeByName

  maybeFindHashTypeByName :: Maybe Old.HashAlgorithm
  maybeFindHashTypeByName = find (\ hashType -> Old.canonicalHashName hashType == name ) [Old.SHA256, Old.MD5, Old.SHA1, Old.SHA512] -- SHA256 is the most used in Nix - so it matches first

  decodeToSomeDigest :: Old.HashAlgorithm -> Text -> Either String (Old.Digest a)
  decodeToSomeDigest a s = goDecode a s

  goDecode :: Old.HashAlgorithm -> Text -> Either String (Old.Digest a)
  goDecode a s =
    -- Base encoding detected by comparing the lengths of the hash in Base to the canonical length of the demanded hash type
    maybe left (`Base.decodeBase` s) maybeFindBaseEncByLenMatch
   where
    left = Left $ Text.unpack sriHash <> " is not a valid " <> Text.unpack name <> " hash. Its length (" <> show (Text.length s) <> ") does not match any of " <> show (canonicalLenIf <$> bases)

    maybeFindBaseEncByLenMatch = find (\ enc -> Text.length s == canonicalLenIf enc) bases

    expectedHashLen = Old.canonicalHashLen a

    canonicalLenIf Base.Base16 = 2 * expectedHashLen
    canonicalLenIf Base.Base32 = ((8 * expectedHashLen - 1) `div` 5) + 1
    canonicalLenIf Base.Base64 = ((4 * expectedHashLen `div` 3) + 3) `div` 4 * 4
    bases = [Base.Base32, Base.Base16, Base.Base64]  -- 32 is the most used in Nix - so the first match
