module Main where

import qualified Test.Hspec
import qualified NixDaemonSpec

-- we run remote tests in
-- Linux namespaces to avoid interacting with systems store
main :: IO ()
main = do
  NixDaemonSpec.enterNamespaces
  Test.Hspec.hspec
    NixDaemonSpec.spec
