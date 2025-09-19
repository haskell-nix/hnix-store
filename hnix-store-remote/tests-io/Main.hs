module Main where

import Test.Hspec qualified
import NixDaemonSpec qualified

-- we run remote tests in
-- Linux namespaces to avoid interacting with systems store
main :: IO ()
main = do
  NixDaemonSpec.enterNamespaces
  Test.Hspec.hspec
    NixDaemonSpec.spec
