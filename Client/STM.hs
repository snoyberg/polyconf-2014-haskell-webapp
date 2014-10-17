{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude
import SharedTypes
import JavaScript.JQuery
import Data.Monoid ((<>))
import Control.Monad (forM_, unless, when, forever)
import Data.Default (def)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson (FromJSON, toJSON, eitherDecodeStrict')
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent.Async (race, async, concurrently)
import Control.Concurrent (threadDelay)
import Data.Text (pack)
import Data.Text.Read (decimal)
import Control.Concurrent.STM

main :: IO ()
main = do
    aliceVar <- newTVarIO 100
    bobVar   <- newTVarIO 100

    button <- select "#give"
    click (const $ giveBob aliceVar bobVar) def button

    concurrently
        (payAlice aliceVar)
        (displayBalance aliceVar bobVar)
    return ()

payAlice :: TVar Int -> IO ()
payAlice aliceVar = forever $ do
    threadDelay 5000000
    atomically $ modifyTVar aliceVar (+ 10)

giveBob :: TVar Int -> TVar Int -> IO ()
giveBob aliceVar bobVar = atomically $ do
    alice <- readTVar aliceVar
    when (alice >= 5) $ do
        writeTVar aliceVar (alice - 5)
        modifyTVar bobVar (+ 5)

displayBalance :: TVar Int -> TVar Int -> IO ()
displayBalance aliceVar bobVar = do
    aliceSpan <- select "#alice"
    bobSpan   <- select "#bob"

    let loop alice bob = do
            setText (pack $ show alice) aliceSpan
            setText (pack $ show bob) bobSpan

            (alice', bob') <- atomically $ do
                alice' <- readTVar aliceVar
                bob'   <- readTVar bobVar
                check (alice /= alice' || bob /= bob')
                return (alice', bob')
            loop alice' bob'

    alice0 <- readTVarIO aliceVar
    bob0 <- readTVarIO bobVar
    loop alice0 bob0
