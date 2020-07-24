{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module System.Nix.Store.Remote.Builders (
    buildContentAddressableAddress
  ) where

import Data.Text.Lazy (Text)
import System.Nix.Hash (Digest, NamedAlgo, SomeNamedDigest(SomeDigest))
import System.Nix.StorePath (ContentAddressableAddress(..), NarHashMode(..))

import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder

import qualified System.Nix.Hash

-- | Marshall `ContentAddressableAddress` to `Text`
-- in form suitable for remote protocol usage.
buildContentAddressableAddress :: forall hashAlgo . NamedAlgo hashAlgo
                               => ContentAddressableAddress
                               -> Text
buildContentAddressableAddress =
  Data.Text.Lazy.Builder.toLazyText . contentAddressableAddressBuilder @hashAlgo

contentAddressableAddressBuilder :: forall hashAlgo . NamedAlgo hashAlgo
                               => ContentAddressableAddress
                               -> Builder
contentAddressableAddressBuilder (Text digest) =
     "text:"
  <> digestBuilder digest
contentAddressableAddressBuilder (Fixed narHashMode (SomeDigest digest)) =
     "fixed:"
  <> (Data.Text.Lazy.Builder.fromText $ System.Nix.Hash.algoName @hashAlgo)
  <> digestBuilder digest

digestBuilder :: Digest a -> Builder
digestBuilder =
    Data.Text.Lazy.Builder.fromText
  . System.Nix.Hash.encodeBase32
