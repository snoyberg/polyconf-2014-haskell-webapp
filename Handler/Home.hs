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
getHomeR = defaultLayout $ do
    setTitle "PolyConf 2014- GHCJS demos"
    [whamlet|
        <ul>
            <li>
                <a href=@{TypeSafeCommR}>Type safe communications
            <li>
                <a href=@{DebounceR}>Debouncing
            <li>
                <a href=@{RaceR}>Race
            <li>
                <a href=@{STMR}>Software transactional memory
    |]

getTypeSafeCommR :: Handler Html
getTypeSafeCommR = do
    master <- getYesod
    defaultLayout $ do
        setTitle "Haskell: Now on the client!"
        $(widgetFile "type-safe-comm")
        addScriptEither $ urlJqueryJs master
        addScript $ StaticR _Home_jsexe_all_js

getDebounceR :: Handler Html
getDebounceR = do
    master <- getYesod
    defaultLayout $ do
        setTitle "Debounce"
        $(widgetFile "debounce")
        addScriptEither $ urlJqueryJs master
        addScript $ StaticR _Debounce_jsexe_all_js

getRaceR :: Handler Html
getRaceR = do
    master <- getYesod
    defaultLayout $ do
        setTitle "Race"
        $(widgetFile "race")
        addScriptEither $ urlJqueryJs master
        addScript $ StaticR _Race_jsexe_all_js

getSTMR :: Handler Html
getSTMR = do
    master <- getYesod
    defaultLayout $ do
        setTitle "Software Transactional Memory"
        $(widgetFile "stm")
        addScriptEither $ urlJqueryJs master
        addScript $ StaticR _STM_jsexe_all_js
