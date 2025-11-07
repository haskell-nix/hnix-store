{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.OutputName where

import System.Nix.OutputName (OutputName)
import Data.Text qualified
import System.Nix.OutputName qualified

import Test.QuickCheck (Arbitrary(arbitrary), choose, elements, vectorOf)

instance Arbitrary OutputName where
  arbitrary =
      either (error . show) id
    . System.Nix.OutputName.mkOutputName
    . Data.Text.pack <$> ((:) <$> s1 <*> limited sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1 = elements $ alphanum <> "+-_?="
    sn = elements $ alphanum <> "+-._?="
    limited n = do
      k <- choose (0, 210)
      vectorOf k n
