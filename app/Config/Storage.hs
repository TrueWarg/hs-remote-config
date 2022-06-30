{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Config.Storage where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Scientific (Scientific)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ConfigBooleanField
    configId ConfigId
    name String
    value Bool
    deriving Show

ConfigNumberField
    configId ConfigId
    name String
    value Int 
    deriving Show

ConfigStringField
    configId ConfigId
    name String
    value String
    deriving Show

ConfigBooleanDescriptor
    configId ConfigId
    name String
    value Bool
    deriving Show

ConfigNumberDescriptor
    configId ConfigId
    name String
    value String
    deriving Show

ConfigStringDescriptor
    configId ConfigId
    name String
    value String
    deriving Show

Config
    version Int
    deriving Show
|]
