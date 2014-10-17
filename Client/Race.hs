{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude
import SharedTypes
import JavaScript.JQuery
import Data.Monoid ((<>))
import Control.Monad (forM_, unless)
import Data.Default (def)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson (FromJSON, toJSON, eitherDecodeStrict')
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)
import Data.Text (pack)
import Data.Text.Read (decimal)

call :: FromJSON a => (Returns a -> Command) -> IO a
call mkCmd = do
    AjaxResult status mdata <- ajax "/command" params settings
    unless (status == 200)
        $ error $ "call failed with status code: " ++ show status
    text <- maybe (error "call returned no data") return mdata
    case eitherDecodeStrict' $ encodeUtf8 text of
        Left s -> error s
        Right v -> return v
  where
    cmd = mkCmd Returns
    cmdText = TL.toStrict $ toLazyText $ encodeToTextBuilder $ toJSON cmd
    params = [("json", cmdText)]
    settings = def
        { asMethod = POST
        }

click' j f = click f def { hsPreventDefault = True } j

main :: IO ()
main = do
    button <- select "button"
    result <- select "#result"
    index  <- select "#index"

    click' button $ \_ -> do
        indexT <- getVal index

        case decimal indexT of
            Right (indexI, "") -> do
                setText "Starting the race" result
                res <- race (expensiveComputation indexI)
                     $ call (ExpensiveComputation indexI)
                setText (pack $ show res) result
            _ -> setText "Invalid input" result
        return ()
    return ()
