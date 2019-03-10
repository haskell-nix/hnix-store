{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds   #-}
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashSet         as HS
import           Data.Maybe
import           Control.Monad.Reader
import           Text.Pretty.Simple
import           Data.Proxy

import qualified System.Nix.GC                as GC
import           System.Nix.Path              (PathHashAlgo)
import           System.Nix.Store.Remote
import           System.Nix.Store.Remote.Util
import           System.Nix.Hash

noSuchPath = fromJust $ mkPath "blah"

main = do
  x <- runStore $ do
    syncWithGC

    verifyStore False False

    (Just path)  <- addTextToStore "hnix-store" "test" (HS.fromList [])  False

    -- (Just path2) <-  addTextToStore "hnix-store2" "test2" (HS.fromList [])  False
    path2 <- addToStore "hi-test-file"
      "/home/greghale/code/hnix-store/hnix-store-remote/hi"
      False (Proxy :: Proxy 'SHA256) (const True) False

    valid <- isValidPathUncached path
    valid2 <- isValidPathUncached path2

    case (valid, valid2) of
      (True, True) -> do
        info <- queryPathInfoUncached path
        info2 <- queryPathInfoUncached path2
        return (path, info, path2, info2)
      _ -> error "shouldn't happen"

  pPrint x
  case x of
    (Left err, log) -> putStrLn err >> print log
    (Right (path, pathinfo, path2, pathinfo2), log) -> do
      gcres <- runStore $ do
        collectGarbage $ GC.Options
          { GC.operation = GC.DeleteSpecific
          , GC.pathsToDelete = HS.fromList [path]
          , GC.ignoreLiveness = False
          -- XXX: this breaks stuff - we don't get Last,
          -- only a message with limit reached, stopping..
          --
          -- gcDeleteSpecific and options.maxFreed
          -- are two distinct branches in
          -- nix/src/libstore.gc
          -- so maybe this combination is not supported
          --, GC.maxFreed = 1000
          , GC.maxFreed = -1
          }

      pPrint gcres

  -- test ExceptT
  e <- runStore $ do
    isValidPathUncached $ noSuchPath

  pPrint e

  {-
  e <- runStore $ do
    queryPathInfoUncached $ noSuchPath

  pPrint e
  -}

    --res <- queryDerivationOutputs drvP
    --liftIO $ print res
    --res <- findRoots
    --liftIO $ pPrint res
