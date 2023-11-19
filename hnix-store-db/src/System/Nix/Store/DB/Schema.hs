{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module System.Nix.Store.DB.Schema where

import Data.Text (Text)
import Data.Word (Word64)

import Database.Persist.TH ( mkMigrate
                           , mkPersist
                           , share
                           , sqlSettings
                           )

import System.Nix.ContentAddress (ContentAddress)
import System.Nix.StorePath (StorePath)
import System.Nix.StorePath.Metadata (StorePathTrust(..))

import System.Nix.Store.DB.Instances (NixUTCTime)
import System.Nix.Store.DB.Util (persistLikeNix)

-- shcema version 10
-- cat /nix/var/nix/db/schema
-- 10

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll" ] [persistLikeNix|
  ValidPath
    path      StorePath
    hash      Text
    regTime   NixUTCTime sql=registrationTime
    deriver   StorePath Maybe
    narBytes  Word64 sql=narSize
    ultimate  StorePathTrust Maybe
    -- ^ null is BuiltElsewhere, 1 is BuiltLocally
    sigs      Text Maybe
    -- ^ space separated
    ca        ContentAddress Maybe
    -- ^ if not null, an assertion that the path is content-addressed
    deriving Eq Show Ord

  Ref
    referrer  ValidPathId
    reference ValidPathId

    Primary referrer reference

    Foreign ValidPath OnDeleteCascade fk_referrer referrer
    Foreign ValidPath OnDeleteRestrict fk_reference reference
    deriving Eq Show Ord

  DerivationOutput
    drv      ValidPathId
    name     Text sql=id
    -- ^ symbolic output id, usually "out"
    path     StorePath

    Primary drv name

    Foreign ValidPath OnDeleteCascade fk_drv drv
    deriving Eq Show Ord
|]
