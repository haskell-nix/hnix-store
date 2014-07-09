module Nix.Settings
    ( -- * The @Settings@ type
      Settings(..)
    ) where

-- | Settings to control various aspects of the behavior of nix.
data Settings = Settings
    { daemonSocketFile :: !FilePath  -- ^ The path to the nix daemon socket.
    }
