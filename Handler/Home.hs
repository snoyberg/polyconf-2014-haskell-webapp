{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Jquery

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    master <- getYesod
    defaultLayout $ do
        setTitle "Haskell: Now on the client!"
        $(widgetFile "homepage")
        addScriptEither $ urlJqueryJs master
        addScript $ StaticR _Home_jsexe_all_js
