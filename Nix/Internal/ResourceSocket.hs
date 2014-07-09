module Nix.Internal.ResourceSocket (
  ResourceSocket,
  allocateResourceSocket,
  connect
) where

import System.Posix.IO (setFdOption, FdOption(CloseOnExec))
import qualified Network.Socket as S
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)

newtype ResourceSocket = ResourceSocket S.Socket

allocateResourceSocket :: MonadResource m
                       => S.Family
                       -> S.SocketType
                       -> S.ProtocolNumber
                       -> m (ReleaseKey, ResourceSocket)
allocateResourceSocket f t p = allocate alloc dealloc
  where
    alloc = do
        socket <- S.socket f t p
        -- Race here! https://github.com/haskell/network/issues/119
        setFdOption (fromIntegral $ S.fdSocket socket) CloseOnExec True
        return $ ResourceSocket socket
    dealloc (ResourceSocket s) = S.close s

unwrap :: ResourceSocket -> S.Socket
unwrap (ResourceSocket s) = s

connect :: ResourceSocket -> S.SockAddr -> IO ()
connect = S.connect . unwrap
