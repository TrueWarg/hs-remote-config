{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import qualified Config.Storage               as Storage
import           Control.Monad                (forM)
import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Database.Persist.Sqlite
import           Yesod

data App =
  App ConnectionPool

mkYesod
  "App"
  [parseRoutes|
/config/register ConfigRegister POST
/config/ Config GET
/config/item Item GET

/admin/config AdminConfig GET POST PATCH
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    App pool <- getYesod
    runSqlPool action pool

postConfigRegister :: Handler Html
postConfigRegister = defaultLayout [whamlet|client/register|]

getConfig :: Handler Html
getConfig = do
  values :: [Entity Storage.ConfigStringField] <- runDB $ selectList [] []
  defaultLayout
    [whamlet|
          <ul>
              $forall Entity id value <- values
                  <li>
                      #{Storage.configStringFieldName value}
      |]

getItem :: Handler Html
getItem = defaultLayout [whamlet|client/item|]

getAdminConfig :: Handler Html
getAdminConfig = defaultLayout [whamlet|client/item|]

postAdminConfig :: Handler Html
postAdminConfig = defaultLayout [whamlet|client/item|]

patchAdminConfig :: Handler Html
patchAdminConfig = defaultLayout [whamlet|client/item|]

openConnectionCount = 3

mockDescriptors :: Storage.ConfigId -> [Storage.ConfigStringDescriptor]
mockDescriptors id =
  [ Storage.ConfigStringDescriptor id "Platform" "Android"
  , Storage.ConfigStringDescriptor id "Build" "23.23"
  , Storage.ConfigStringDescriptor id "Name" "Ali"
  ]

mockFields :: Storage.ConfigId -> [Storage.ConfigStringField]
mockFields id =
  [ Storage.ConfigStringField id "use_it" "it"
  , Storage.ConfigStringField id "make_so" "so"
  ]

main :: IO ()
main =
  runStderrLoggingT $
  withSqlitePool ":memory:" openConnectionCount $ \pool ->
    liftIO $ do
      runResourceT $
        flip runSqlPool pool $ do
          runMigration Storage.migrateAll
          storageId <- insert $ Storage.Config 1
          forM (mockDescriptors storageId) insert
          forM (mockFields storageId) insert
      warp 3000 $ App pool
