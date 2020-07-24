import Test.Tasty.Hspec
import NixDaemon

-- we run remote tests in
-- Linux namespaces to avoid interacting with systems store
main = do
  enterNamespaces
  hspec spec_protocol
