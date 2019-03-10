--{-# OPTIONS_GHC -F -pgmF tasty-discover #-}

import           Test.Tasty.Hspec

import NixDaemon
main = enterNamespaces >> hspec spec_protocol
--main = hspec spec_protocol
