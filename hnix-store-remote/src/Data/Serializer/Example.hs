{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Serializer.Example
  (
  -- * Simple protocol
    OpCode(..)
  , Cmd(..)
  -- * Cmd Serializer
  , cmdS
  -- * Runners
  , runG
  , runP
  -- * Custom errors
  , MyGetError(..)
  -- ** Erroring variants of cmdS
  -- *** getS with throwError and MyGetError
  , cmdSGetError
  -- *** getS with fail
  , cmdSGetFail
  -- * Elaborate
  , cmdSRest
  , runGRest
  , runPRest
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Int (Int8)
import Data.GADT.Show (GShow(..), defaultGshowsPrec)
import Data.Kind (Type)
import Data.Type.Equality (TestEquality(..), (:~:)(Refl))
import Data.Serialize.Get (Get, getInt8)
import Data.Serialize.Put (Putter, PutM, putInt8)
import Data.Serializer
  ( Serializer(..)
  , GetSerializerError
  , runGetS
  , runPutS
  , transformGetError
  )
import Data.Some (Some(..))
import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary(..), oneof)

-- * Simple protocol

-- | OpCode used to differentiate between operations
data OpCode = OpCode_Int | OpCode_Bool
  deriving (Bounded, Eq, Enum, Generic, Ord, Show)

-- | Protocol operations
data Cmd :: Type -> Type where
  Cmd_Int :: Int8 -> Cmd Int8
  Cmd_Bool :: Bool -> Cmd Bool

deriving instance Eq (Cmd a)
deriving instance Show (Cmd a)

instance GShow Cmd where
  gshowsPrec = defaultGshowsPrec

instance TestEquality Cmd where
    testEquality (Cmd_Int _) (Cmd_Int _) = Just Refl
    testEquality (Cmd_Bool _) (Cmd_Bool _) = Just Refl
    testEquality _ _ = Nothing

-- constructors only
-- import Data.GADT.Compare
-- instance GEq Cmd where
--   geq = testEquality

instance {-# OVERLAPPING #-} Eq (Some Cmd) where
  Some (Cmd_Int a) == Some (Cmd_Int b) = a == b
  Some (Cmd_Bool a) == Some (Cmd_Bool b) = a == b
  _ == _ = False

instance Arbitrary (Some Cmd) where
  arbitrary = oneof
    [ Some . Cmd_Int <$> arbitrary
    , Some . Cmd_Bool <$> arbitrary
    ]

-- | @OpCode@ @Serializer@
opcode :: MonadTrans t => Serializer t OpCode
opcode = Serializer
  { getS = lift getEnum
  , putS = putEnum
  }

-- * Cmd Serializer

-- | @Cmd@ @Serializer@
cmdS
  :: forall t
  . ( MonadTrans t
    , Monad (t Get)
    )
  => Serializer t (Some Cmd)
cmdS = Serializer
  { getS = getS opcode >>= \case
      OpCode_Int -> Some . Cmd_Int <$> lift getInt8
      OpCode_Bool -> Some . Cmd_Bool <$> lift getBool
  , putS = \case
      Some (Cmd_Int i) -> putS (opcode @t) OpCode_Int >> putInt8 i
      Some (Cmd_Bool b) -> putS (opcode @t) OpCode_Bool >> putBool b
  }

-- * Runners

-- | @runGetS@ specialized to @ExceptT e@
runG
  :: Serializer (ExceptT e) a
  -> ByteString
  -> Either (GetSerializerError e) a
runG s =
  transformGetError
  . runGetS s runExceptT

-- | @runPutS@ specialized to @ExceptT e@
runP
  :: Serializer (ExceptT e) a
  -> a
  -> ByteString
runP = runPutS

-- * Custom errors

data MyGetError
  = MyGetError_Example
  deriving (Eq, Show)

-- ** Erroring variants of cmdS

-- *** getS with throwError and MyGetError

cmdSGetError :: Serializer (ExceptT MyGetError) (Some Cmd)
cmdSGetError = Serializer
  { getS = getS opcode >>= \case
      OpCode_Int -> Some . Cmd_Int <$> lift getInt8
      OpCode_Bool -> throwError MyGetError_Example
  , putS = putS $ cmdS @(ExceptT MyGetError)
  }

-- *** getS with fail

cmdSGetFail
  :: forall t
  .  ( MonadTrans t
     , MonadFail (t Get)
     , Monad (t PutM)
     )
  => Serializer t (Some Cmd)
cmdSGetFail = Serializer
  { getS = getS opcode >>= \case
      OpCode_Int -> Some . Cmd_Int <$> lift getInt8
      OpCode_Bool -> fail "no parse"
  , putS = putS $ cmdS @t
  }

-- * Elaborate

-- | Transformer for @Serializer@
newtype REST r e s m a = REST
  { _unREST :: ExceptT e (StateT s (ReaderT r m)) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError e
    , MonadReader r
    , MonadState s
    , MonadFail
    )

instance MonadTrans (REST r e s) where
  lift = REST . lift . lift . lift

-- | Runner for @REST@
restRunner
  :: Monad m
  => r
  -> s
  -> REST r e s m a
  -> m ((Either e a), s)
restRunner r s =
    (`runReaderT` r)
  . (`runStateT` s)
  . runExceptT
  . _unREST

runGRest
  :: Serializer (REST r e s) a
  -> r
  -> s
  -> ByteString
  -> Either (GetSerializerError e) a
runGRest serializer r s =
    transformGetError
  . second fst
  . runGetS
      serializer
      (restRunner r s)

runPRest
  :: Serializer (REST r e s) a
  -> a
  -> ByteString
runPRest = runPutS

cmdSRest
  :: forall t e
  .  t ~ REST Bool e Int
  => Bool
  -> Serializer t (Some Cmd)
cmdSRest isTrue = Serializer
  { getS = getS opcode >>= \case
      OpCode_Int -> do
        if isTrue
        then Some . Cmd_Int . (+1) <$> lift getInt8
        else Some . Cmd_Int <$> lift getInt8
      OpCode_Bool -> Some . Cmd_Bool <$> lift getBool
  , putS = \case
      Some (Cmd_Int i) -> do
        putS (opcode @t) OpCode_Int
        if isTrue
        then putInt8 (i - 1)
        else putInt8 i
      Some (Cmd_Bool b) -> putS (opcode @t) OpCode_Bool >> putBool b
  }

-- Primitives helpers

getInt :: Integral a => Get a
getInt = fromIntegral <$> getInt8

putInt :: Integral a => Putter a
putInt = putInt8 . fromIntegral

-- | Deserialize @Bool@ from integer
getBool :: Get Bool
getBool = (getInt :: Get Int8) >>= \case
  0 -> pure False
  1 -> pure True
  x -> fail $ "illegal bool value " ++ show x

-- | Serialize @Bool@ into integer
putBool :: Putter Bool
putBool True  = putInt (1 :: Int8)
putBool False = putInt (0 :: Int8)

-- | Utility toEnum version checking bounds using Bounded class
toEnumCheckBounds
  :: forall a
   . ( Bounded a
     , Enum a
     )
  => Int
  -> Either String a
toEnumCheckBounds = \case
  x | x < fromEnum (minBound @a) -> Left $ "enum out of min bound " ++ show x
  x | x > fromEnum (maxBound @a) -> Left $ "enum out of max bound " ++ show x
  x | otherwise -> Right $ toEnum x

-- | Deserialize @Enum@ to integer
getEnum
  :: ( Bounded a
     , Enum a
     )
  => Get a
getEnum =
  toEnumCheckBounds <$> getInt
  >>= either fail pure

-- | Serialize @Enum@ to integer
putEnum :: Enum a => Putter a
putEnum = putInt . fromEnum
