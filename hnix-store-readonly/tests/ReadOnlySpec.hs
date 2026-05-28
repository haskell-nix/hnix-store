{-# LANGUAGE OverloadedStrings #-}

module ReadOnlySpec where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = do
  describe "ReadOnly" $ do
    describe "computeStorePathForPath" $ do
      it "computes correct StorePath" $
        pendingWith "needs IO and a sample directory to add"
