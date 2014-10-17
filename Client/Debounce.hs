{-# LANGUAGE OverloadedStrings #-}
import JavaScript.JQuery
import Data.Time
import Data.Default (def)
import Data.Text (pack)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    button <- select "<button class='btn'>Click me</button>"
    body <- select "#main"
    appendJQuery button body
    ul <- select "<ul>"
    appendJQuery ul body

    signal <- newEmptyMVar
    forkIO $ forever $ do
        takeMVar signal

        now <- getCurrentTime
        li <- select "<li/>"
        setText (pack $ show now) li
        appendJQuery li ul

        threadDelay 1000000

    click (\_ -> void $ tryPutMVar signal ()) def
        { hsPreventDefault = True
        } button
    return ()
