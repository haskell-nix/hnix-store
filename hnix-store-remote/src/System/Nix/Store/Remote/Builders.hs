{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module System.Nix.Store.Remote.Builders
  ( buildContentAddressableAddress
  )
where

import           Data.Text.Lazy                 ( Text )
import           Crypto.Hash                    ( Digest )
import           System.Nix.StorePath           ( ContentAddressableAddress(..)
                                                )

import           Data.Text.Lazy.Builder         ( Builder )
import qualified Data.Text.Lazy.Builder

import           System.Nix.Hash

-- | Marshall `ContentAddressableAddress` to `Text`
-- in form suitable for remote protocol usage.
buildContentAddressableAddress :: ContentAddressableAddress -> Text
buildContentAddressableAddress =
  Data.Text.Lazy.Builder.toLazyText . contentAddressableAddressBuilder

contentAddressableAddressBuilder :: ContentAddressableAddress -> Builder
contentAddressableAddressBuilder (Text digest) =
  "text:" <> digestBuilder digest
contentAddressableAddressBuilder (Fixed _narHashMode (SomeDigest (digest :: Digest hashAlgo))) =
  "fixed:"
  <> Data.Text.Lazy.Builder.fromText (System.Nix.Hash.algoName @hashAlgo)
  <> digestBuilder digest

digestBuilder :: Digest a -> Builder
digestBuilder =
  Data.Text.Lazy.Builder.fromText . encodeDigestWith NixBase32
