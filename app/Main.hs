{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Yesod

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/config/register ConfigRegister POST
/config/ Config GET
/config/item Item GET

/admin/config AdminConfig GET POST PATCH
|]

instance Yesod App

postConfigRegister :: Handler Html
postConfigRegister = defaultLayout [whamlet|client/register|]

getConfig :: Handler Html
getConfig = defaultLayout [whamlet|client/config|]

getItem :: Handler Html
getItem = defaultLayout [whamlet|client/item|]

getAdminConfig :: Handler Html
getAdminConfig = defaultLayout [whamlet|client/item|]

postAdminConfig :: Handler Html
postAdminConfig = defaultLayout [whamlet|client/item|]

patchAdminConfig :: Handler Html
patchAdminConfig = defaultLayout [whamlet|client/item|]

main :: IO ()
main = warp 3000 App
