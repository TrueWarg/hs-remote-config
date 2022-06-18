{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Main where

import Yesod

data App = App -- Put your config, database connection pool, etc. in here.

-- Derive routes and instances for App.
mkYesod "App" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod App -- Methods in here can be overridden as needed.

-- The handler for the GET request at /, corresponds to HomeR.
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Lol Kek Chebureck|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go home|]

main :: IO ()
main = warp 3000 App
