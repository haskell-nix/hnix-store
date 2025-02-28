{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Placeholder
  ( Placeholder
  , createPlaceholder
  , renderPlaceholder

  , DownstreamPlaceholder
  , renderDownstreamPlaceholder
  , unknownCaOutput
  , unknownDerivation
  , downstreamPlaceholderFromSingleDerivedPathBuilt
  ) where

import Data.ByteArray qualified
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import System.Nix.Base
import System.Nix.Hash
import System.Nix.StorePath
import System.Nix.DerivedPath
import System.Nix.OutputName
import System.Nix.Hash.Truncation

-- | For a derivation's own outputs
newtype Placeholder = Placeholder
  { placeholder_hash :: Digest SHA256
  } deriving (Show, Eq)

-- | Given an output name, make a placeholder
createPlaceholder :: OutputName -> Placeholder
createPlaceholder outputName =
  let
    clearText = T.intercalate ":"
      [ "nix-output"
      , unStorePathName $ unOutputName outputName
      ]
  in Placeholder (Crypto.Hash.hash $ T.encodeUtf8 clearText)

-- | This creates an opaque and almost certainly unique string
-- deterministically from the placeholder.
renderPlaceholder :: Placeholder -> Text
renderPlaceholder (Placeholder h) = T.cons '/' (encodeDigestWith NixBase32 h)

-- | Downstream Placeholders are opaque and almost certainly unique
-- values used to allow derivations to refer to store objects which are
-- yet to be built and for we do not yet have store paths for.
--
-- They correspond to `SingleDerivedPath`s that are not
-- `SingleDerivedPath_Opaque`, except for the cases involving input
-- addressing or fixed outputs where we do know a store path for the
-- derivation output in advance.
--
-- Unlike `SingleDerivedPath`, however, `DownstreamPlaceholder` is
-- purposefully opaque and obfuscated. This is so they are hard to
-- create by accident, and so substituting them (once we know what the
-- path to store object is) is unlikely to capture other stuff it
-- shouldn't.
--
-- We use them with `Derivation`: the `render()` method is called to
-- render an opaque string which can be used in the derivation, and the
-- resolving logic can substitute those strings for store paths when
-- resolving `Derivation.inputs.drvs` to `BasicDerivation.input.srcs`.
newtype DownstreamPlaceholder = DownstreamPlaceholder
  { downstreamPlaceholder_hash :: Digest SHA256
  } deriving (Show, Eq)

-- | This creates an opaque and almost certainly unique string
-- deterministically from the placeholder.
renderDownstreamPlaceholder :: DownstreamPlaceholder -> Text
renderDownstreamPlaceholder (DownstreamPlaceholder h) = T.cons '/' (encodeDigestWith NixBase32 h)

-- | Create a placeholder for an unknown output of a content-addressed
-- derivation.
--
-- The derivation itself is known (we have a store path for it), but
-- the output doesn't yet have a known store path.
unknownCaOutput :: StorePath -> OutputName -> DownstreamPlaceholder
unknownCaOutput drvPath outputName =
  let
    clearText = T.intercalate ":"
      [ "nix-upstream-output"
      , storePathHashPartToText $ storePathHash drvPath
      , either (error . show) unStorePathName $ do
          name <- mkStorePathName $ T.dropEnd 4 (unStorePathName $ storePathName drvPath) -- Remove ".drv" extension
          outputStoreObjectName name outputName
      ]
  in DownstreamPlaceholder (Crypto.Hash.hash $ T.encodeUtf8 clearText)

-- | Create a placehold for the output of an unknown derivation.
--
-- The derivation is not yet known because it is a dynamic
-- derivaiton --- it is itself an output of another derivation ---
-- and we just have (another) placeholder for it.
unknownDerivation :: DownstreamPlaceholder -> OutputName -> DownstreamPlaceholder
unknownDerivation (DownstreamPlaceholder h) outputName =
  let
    clearText = T.intercalate ":"
      [ "nix-computed-output"
      , encodeWith NixBase32 $ System.Nix.Hash.Truncation.truncateInNixWay 20 $ Data.ByteArray.convert h
      , unStorePathName $ unOutputName outputName
      ]
  in DownstreamPlaceholder (Crypto.Hash.hash $ T.encodeUtf8 clearText)

-- | Convenience constructor that handles both cases (unknown
-- content-addressed output and unknown derivation), delegating as
-- needed to `unknownCaOutput` and `unknownDerivation`.
--
-- Recursively builds up a placeholder from a
-- `SingleDerivedPath::Built.drvPath` chain.
downstreamPlaceholderFromSingleDerivedPathBuilt :: SingleDerivedPath -> OutputName -> DownstreamPlaceholder
downstreamPlaceholderFromSingleDerivedPathBuilt drvPath outputName = case drvPath of
  SingleDerivedPath_Opaque drvPath' ->
    unknownCaOutput drvPath' outputName
  SingleDerivedPath_Built drvPath' outputName' ->
    unknownDerivation (downstreamPlaceholderFromSingleDerivedPathBuilt drvPath' outputName') outputName
