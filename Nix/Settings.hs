module Nix.Settings (
  Settings(..)
) where

data Settings = Settings {
  daemonSocketFile :: FilePath
}
