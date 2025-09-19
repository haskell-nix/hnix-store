{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module System.Nix.Store.DB.Instances where

import Database.Persist (PersistField(..), PersistValue(..), SqlType(..))
import Database.Persist.Sql (PersistFieldSql(..))

import Data.Time (UTCTime)
import Data.Default.Class (Default(def))

import System.Nix.ContentAddress (ContentAddress)
import System.Nix.StorePath (StorePath)
import System.Nix.StorePath.Metadata (StorePathTrust(..))

import Data.Attoparsec.Text qualified
import Data.Bifunctor qualified
import Data.Text qualified
import Data.Time.Clock.POSIX qualified

import System.Nix.ContentAddress qualified
import System.Nix.StorePath qualified

instance PersistField StorePath where
  toPersistValue = PersistText . System.Nix.StorePath.storePathToText def
  fromPersistValue (PersistText t) = either
    (Left . Data.Text.pack)
    Right
    $ Data.Attoparsec.Text.parseOnly
      (System.Nix.StorePath.pathParser def)
      t
  fromPersistValue wrongValue = Left
    $ "Received "
    <> (Data.Text.pack $ show wrongValue)
    <> " when a value of type PersistText was expected."

instance PersistFieldSql StorePath where
  sqlType _ = SqlString

instance PersistField StorePathTrust where
  toPersistValue BuiltLocally     = PersistInt64 1
  toPersistValue BuiltElsewhere   = PersistNull

  fromPersistValue (PersistInt64 1) = pure BuiltLocally
  fromPersistValue PersistNull      = pure BuiltElsewhere
  fromPersistValue wrongValue = Left
    $ "Received "
    <> (Data.Text.pack $ show wrongValue)
    <> " when a value of type PersistNull"
    <> " or (PersistInt64 1) was expected."

instance PersistFieldSql StorePathTrust where
  sqlType _ = SqlInt64

newtype NixUTCTime = NixUTCTime UTCTime
  deriving (Eq, Show, Ord)

instance PersistField NixUTCTime where
  toPersistValue (NixUTCTime u) = PersistInt64
    $ round $  Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds u
  fromPersistValue (PersistInt64 i) = pure $ NixUTCTime
    $ Data.Time.Clock.POSIX.posixSecondsToUTCTime $ fromIntegral i
  fromPersistValue wrongValue = Left
    $ "Received "
    <> (Data.Text.pack $ show wrongValue)
    <> " when a value of (PersistInt64 _) was expected."

instance PersistFieldSql NixUTCTime where
  sqlType _ = SqlInt64

instance PersistField ContentAddress where
  toPersistValue =
    PersistText
    . System.Nix.ContentAddress.buildContentAddress

  fromPersistValue (PersistText t) =
    Data.Bifunctor.first (\e -> error $ show (e, t))
    $ System.Nix.ContentAddress.parseContentAddress t
  fromPersistValue wrongValue = Left
    $ "Received "
    <> (Data.Text.pack $ show wrongValue)
    <> " when a value of type PersistText was expected."

instance PersistFieldSql ContentAddress where
  sqlType _ = SqlString
