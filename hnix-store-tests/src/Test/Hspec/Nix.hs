module Test.Hspec.Nix
  ( forceRight
  , roundtrips
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

forceRight
  :: Show a
  => Either a b
  -> b
forceRight = \case
  Right x -> x
  Left e -> error $ "forceRight failed: " ++ show e
