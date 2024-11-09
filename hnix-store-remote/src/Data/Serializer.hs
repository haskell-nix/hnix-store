{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description : Serializer data type
Copyright   : (c) John Ericson, 2023
                  Sorki, 2023
Stability   : experimental

@Serializer@ ties @Get@ and @PutM@ monads
into a single datatype and allows
transforming both monads with a monad transformer
for adding extra layers like @ExceptT@
(for example when @putS@ can fail due to unsupported
version of a protocol) or @ReaderT@ (when we need
to serialize a data type based differently based
on a protocol version).

See "Data.Serializer.Example"
-}

module Data.Serializer
  (
  -- * Serializer
    Serializer(..)
  -- ** Runners
  , runGetS
  , runPutS
  -- * Simple serializer
  , SimpleSerializer
  -- ** Simple runners
  , runGetSimple
  -- * From Get/Put, Serialize
  , lift2
  , liftSerialize
  -- * Combinators
  , AlmostPrism(..)
  , maybeAlmostPrism
  , mapIsoSerializer
  , mapPrismSerializer
  , tup
  -- * Utility
  , GetSerializerError(..)
  , transformGetError
  -- * Re-exports
  , Get
  , PutM
  ) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad.Morph
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Serialize (Serialize)
import qualified Data.Serialize
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Putter, PutM, runPutM)
import Data.Traversable


-- * Serializer

-- | @Serializer@ ties @Get@ and @PutM@ monads
-- into a single datatype and allows
-- transforming the monads with a monad transformer
-- for e.g. adding @ExceptT@ or @ReaderT@ layers.
data Serializer t a = Serializer
  { getS :: t Get a
  , putS :: a -> PutM ()
  }

-- ** Runners

-- | Runner for putS of @Serializer@
runPutS
  :: Serializer t a        -- ^ Serializer
  -> a                     -- ^ Value to (out)put
  -> ByteString
runPutS s = snd . runPutM . putS s

-- | Runner for getS of @Serializer@
runGetS
  :: ( Monad (t Get)
     , MonadTrans t
     )
  => Serializer t a     -- ^ Serializer
  -> (t Get a -> Get b) -- ^ Tranformer runner
  -> ByteString         -- ^ ByteString to parse
  -> Either String b
runGetS s run b = runGet (run (getS s)) b

-- * Simple serializer

-- | Simple @Serializer@
type SimpleSerializer a = Serializer IdentityT a

-- ** Simple runners

-- | Runner for getS of @SimpleSerializer@
runGetSimple
  :: SimpleSerializer a
  -> ByteString
  -> Either String a
runGetSimple s b =
  runGetS s (runIdentityT) b

-- * From Get/Put, Serialize

-- | Lift @Get a@ and @Putter a@ into @Serializer@
lift2
  :: forall a t
   . MonadTrans t
  => Get a
  -> Putter a
  -> Serializer t a
lift2 f g = Serializer
  { getS = lift f
  , putS = g
  }

-- | Lift @Serialize a@ instance into @Serializer@
liftSerialize
  :: ( Serialize a
     , MonadTrans t
     )
  => Serializer t a
liftSerialize =
  lift2
    Data.Serialize.get
    Data.Serialize.put

-- * Combinators

-- | Map over @Serializer@
mapIsoSerializer
  :: Functor (t Get)
  => (a -> b) -- ^ Map over @getS@
  -> (b -> a) -- ^ Map over @putS@
  -> Serializer t a
  -> Serializer t b
mapIsoSerializer f g s = Serializer
  { getS = f <$> getS s
  , putS = putS s . g
  }

data AlmostPrism t a b = AlmostPrism
  { _almostPrism_get :: a -> t Identity b
  -- ^ Map over @getS@
  , _almostPrism_put :: b -> a
  -- ^ Map over @putS@
  }

maybeAlmostPrism
  :: Applicative (t Identity)
  => AlmostPrism t a b
  -> AlmostPrism t (Maybe a) (Maybe b)
maybeAlmostPrism ap = AlmostPrism
  { _almostPrism_get = traverse $ _almostPrism_get ap
  , _almostPrism_put = fmap $ _almostPrism_put ap
  }

-- | Map over @Serializer@ where @getS@
-- can return @Either@
mapPrismSerializer
  :: ( Monad (t Get)
     , MFunctor t
     )
  => AlmostPrism t a b
  -> Serializer t a
  -> Serializer t b
mapPrismSerializer p s = Serializer
  { getS = hoist generalize . _almostPrism_get p =<< getS s
  , putS = putS s . _almostPrism_put p
  }

-- | Tuple combinator
tup
  :: ( Applicative (t Get)
     , Monad (t PutM)
     )
  => Serializer t a
  -> Serializer t b
  -> Serializer t (a, b)
tup a b = Serializer
  { getS = liftA2 (,) (getS a) (getS b)
  , putS = \(x, y) -> do
      putS a x
      putS b y
  }

-- * Utilities

-- | Wrapper for both GetS errors
--
--   * the one that occurs when @fail@ is called
--   * custom one when @ExceptT@ is used
data GetSerializerError customGetError
  = SerializerError_GetFail String
  | SerializerError_Get customGetError
  deriving (Eq, Ord, Show)

-- | Helper for transforming nested Eithers
-- into @GetSerializerError@ wrapper
transformGetError
  :: Either String (Either customGetError b)
  -> Either (GetSerializerError customGetError) b
transformGetError = \case
  Left stringyRunGetError -> Left (SerializerError_GetFail stringyRunGetError)
  Right (Left myGetError) -> Left (SerializerError_Get myGetError)
  Right (Right res) -> Right res
