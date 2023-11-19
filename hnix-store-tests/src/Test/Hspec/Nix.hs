module Test.Hspec.Nix
  ( roundtrips
  ) where

import Test.Hspec (Expectation, shouldBe)

roundtrips
  :: forall a b f
   . ( Applicative f
     , Eq (f a)
     , Show a
     , Show b
     , Show (f a)
     )
  => (a -> b)   -- ^ Encode
  -> (b -> f a) -- ^ Decode
  -> a
  -> Expectation
roundtrips encode decode x =
  decode (encode x) `shouldBe` pure x
