{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description : NixSerializer
Copyright   : (c) John Ericson, 2023
                  Richard Marko, 2023
|-}

module System.Nix.Store.Remote.Serializer
  (
  -- * NixSerializer
    NixSerializer
  -- ** Runners
  , runG
  , runP
  -- * Primitives
  , int
  , bool
  , byteString
  , enum
  , text
  , maybeText
  , time
  -- * Combinators
  , list
  , set
  , hashSet
  , mapS
  -- * Lifted from Serialize
  , buildResult
  -- ** Logger
  , activityID
  , maybeActivity
  , activityResult
  , field
  , verbosity
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)

import qualified Control.Monad
import qualified Data.HashSet
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text

import Data.Serializer
import System.Nix.Build (BuildResult)
import System.Nix.Store.Remote.Serialize () -- TODO: getDerivation
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Types

-- | Transformer for @Serializer@
newtype SerialT r e m a = SerialT
  { _unSerialT :: ExceptT e (ReaderT r m) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError e
    , MonadReader r
    , MonadFail
    )

instance MonadTrans (SerialT r e) where
  lift = SerialT . lift . lift

-- | Runner for @SerialT@
runSerialT
  :: Monad m
  => r
  -> SerialT r e m a
  -> m (Either e a)
runSerialT r =
    (`runReaderT` r)
  . runExceptT
  . _unSerialT

-- * NixSerializer

type NixSerializer r e = Serializer (SerialT r e)

-- ** Runners

runG
  :: NixSerializer r e a
  -> r
  -> ByteString
  -> Either (GetSerializerError e) a
runG serializer r =
    transformGetError
  . runGetS
      serializer
      (runSerialT r)

runP
  :: NixSerializer r e a
  -> r
  -> a
  -> Either e ByteString
runP serializer r =
    transformPutError
  . runPutS
      serializer
      (runSerialT r)

-- * Primitives

int :: Integral a => NixSerializer r e a
int = lift2 getInt putInt

bool :: NixSerializer r e Bool
bool = lift2 getBool putBool

byteString :: NixSerializer r e ByteString
byteString = lift2 getByteString putByteString

enum :: Enum a => NixSerializer r e a
enum = lift2 getEnum putEnum

text :: NixSerializer r e Text
text = liftSerialize

maybeText :: NixSerializer r e (Maybe Text)
maybeText = mapIsoSerializer
  (\case
    t | Data.Text.null t -> Nothing
    t | otherwise -> Just t
  )
  (Prelude.maybe mempty id)
  text

time :: NixSerializer r e UTCTime
time = lift2 getTime putTime

-- * Combinators

list
  :: NixSerializer r e a
  -> NixSerializer r e [a]
list s = Serializer
  { getS = do
      count <- getS int
      Control.Monad.replicateM count (getS s)
  , putS = \xs -> do
      putS int (length xs)
      mapM_ (putS s) xs
  }

set
  :: Ord a
  => NixSerializer r e a
  -> NixSerializer r e (Set a)
set =
  mapIsoSerializer
    Data.Set.fromList
    Data.Set.toList
  . list

hashSet
  :: ( Eq a
     , Hashable a
     )
  => NixSerializer r e a
  -> NixSerializer r e (HashSet a)
hashSet =
  mapIsoSerializer
    Data.HashSet.fromList
    Data.HashSet.toList
  . list

mapS
  :: Ord k
  => NixSerializer r e k
  -> NixSerializer r e v
  -> NixSerializer r e (Map k v)
mapS k v =
  mapIsoSerializer
    Data.Map.Strict.fromList
    Data.Map.Strict.toList
  $ list
  $ tup k v

-- * Lifted from Serialize

buildResult :: NixSerializer r e BuildResult
buildResult = liftSerialize

-- ** Logger

maybeActivity :: NixSerializer r e (Maybe Activity)
maybeActivity = Serializer
  { getS = getS (int @Int) >>= \case
      0 -> pure Nothing
      x -> either fail (pure . Just) $ toEnumCheckBounds (x - 100)
  , putS = \case
      Nothing -> putS (int @Int) 0
      Just act -> putS activity act
  }
  where
    activity :: NixSerializer r e Activity
    activity = liftSerialize

activityID :: NixSerializer r e ActivityID
activityID = liftSerialize

activityResult :: NixSerializer r e ActivityResult
activityResult = liftSerialize

field :: NixSerializer r e Field
field = liftSerialize

verbosity :: NixSerializer r e Verbosity
verbosity = liftSerialize
