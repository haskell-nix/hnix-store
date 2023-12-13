module System.Nix.Store.Remote.Types.NoReply
  ( NoReply(..)
  ) where

-- | Reply type for the case where the server does not reply
data NoReply = NoReply
  deriving (Show)

