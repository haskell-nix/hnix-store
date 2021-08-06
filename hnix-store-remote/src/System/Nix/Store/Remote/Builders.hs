{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}

module System.Nix.Store.Remote.Builders
  ( buildContentAddressableAddress
  )
where

import qualified Data.Text.Lazy              as TL
import           Crypto.Hash                    ( Digest )
import           System.Nix.StorePath           ( ContentAddressableAddress(..)
                                                )

import           Data.Text.Lazy.Builder         ( Builder )
import qualified Data.Text.Lazy.Builder      as TL

import           System.Nix.Hash

-- | Marshall `ContentAddressableAddress` to `Text`
-- in form suitable for remote protocol usage.
buildContentAddressableAddress :: ContentAddressableAddress -> TL.Text
buildContentAddressableAddress =
  TL.toLazyText . contentAddressableAddressBuilder

contentAddressableAddressBuilder :: ContentAddressableAddress -> Builder
contentAddressableAddressBuilder (Text digest) =
  "text:" <> digestBuilder digest
contentAddressableAddressBuilder (Fixed _narHashMode (SomeDigest (digest :: Digest hashAlgo))) =
  "fixed:"
  <> TL.fromText (System.Nix.Hash.algoName @hashAlgo)
  <> digestBuilder digest

digestBuilder :: Digest a -> Builder
digestBuilder =
  TL.fromText . encodeDigestWith NixBase32
