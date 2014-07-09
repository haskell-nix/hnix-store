module Nix.Internal.ResourceSocket
    ( -- * The @ResourceSocket@ type
      ResourceSocket
    , allocateResourceSocket

      -- * Socket operations
    , connect
    ) where

import System.Posix.IO
    ( setFdOption
    , FdOption
        ( CloseOnExec
        )
    )
import qualified Network.Socket as S

import Control.Monad.Trans.Resource
    ( MonadResource
    , ReleaseKey
    , allocate
    )

-- | A wrapper around "Network.Socket.Socket" that can only be constructed
-- inside of a MonadResource with a registered cleanup action.
newtype ResourceSocket = ResourceSocket S.Socket

-- | Allocate a @Network.Socket.Socket@ into a @MonadResource@, registering a
-- cleanup action which closes the socket.
allocateResourceSocket :: MonadResource m
                       => S.Family          -- ^ The socket address family
                       -> S.SocketType      -- ^ The socket type
                       -> S.ProtocolNumber  -- ^ The protocol number
                       -> m (ReleaseKey, ResourceSocket)
allocateResourceSocket f t p = allocate alloc dealloc
  where
    alloc = do
        socket <- S.socket f t p
        -- Race here! https://github.com/haskell/network/issues/119
        setFdOption (fromIntegral $ S.fdSocket socket) CloseOnExec True
        return $ ResourceSocket socket
    dealloc (ResourceSocket s) = S.close s

-- | Internal function for cleaner wrapping of @Network.Socket@ functions
unwrap :: ResourceSocket -> S.Socket
unwrap (ResourceSocket s) = s

-- | Connect to a remote socket address
connect :: ResourceSocket  -- ^ The socket to connect
        -> S.SockAddr      -- ^ The socket address
        -> IO ()
connect = S.connect . unwrap
