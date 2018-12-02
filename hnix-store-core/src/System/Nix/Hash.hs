{-|
Description : Cryptographic hashes for hnix-store.
Maintainer  : Shea Levy <shea@shealevy.com>; Greg Hale <imalsogreg@gmail.com>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
module System.Nix.Hash (
    HNix.Digest

  , HNix.HashAlgorithm
  , HNix.HashAlgorithm'(..)
  , HNix.AlgoVal(..)
  , HNix.HasDigest(..)
  , HNix.hash
  , HNix.hashLazy

  , HNix.printAsBase16
  , HNix.printAsBase32
  , HNix.printHashAlgo
  ) where

import qualified System.Nix.Internal.Hash as HNix

