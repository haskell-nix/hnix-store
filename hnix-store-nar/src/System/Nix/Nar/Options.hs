{-# LANGUAGE OverloadedStrings #-}
module System.Nix.Nar.Options
  ( NarOptions(..)
  , defaultNarOptions
  , caseHackSuffix
  ) where

import Data.Text (Text)
import System.Info qualified

-- | Options for configuring how NAR files are encoded and decoded.
data NarOptions = NarOptions {
  optUseCaseHack :: Bool
  -- ^ Whether to enable a case hack to support case-insensitive filesystems.
  -- Equivalent to the 'use-case-hack' option in the Nix client.
  --
  -- The case hack rewrites file names to avoid collisions on case-insensitive file systems, e.g. APFS and HFS+ on macOS.
  -- Enabled by default on macOS (Darwin).
}

defaultNarOptions :: NarOptions
defaultNarOptions = NarOptions {
  optUseCaseHack = System.Info.os == "darwin"
}

caseHackSuffix :: Text
caseHackSuffix = "~nix~case~hack~"
