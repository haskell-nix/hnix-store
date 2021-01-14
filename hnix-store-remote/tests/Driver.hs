import NixDaemon
import qualified Spec

-- we run remote tests in
-- Linux namespaces to avoid interacting with systems store
main :: IO ()
main = do
  enterNamespaces
  Spec.main
