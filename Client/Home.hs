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
    title   <- select "#title"
    content <- select "#content"
    addPost <- select "#add-post"
    message <- select "#message"
    update  <- select "#update"
    posts   <- select "#posts"
    h1      <- select "#display > h1"
    article <- select "#display > article"

    click' addPost $ \event -> do
        preventDefault event
        setText "Adding post..." message
        title' <- getVal title
        content' <- getVal content
        _ <- call (AddPost (Title title') (Content content'))
        setText "New post added" message
        return ()

    click' update $ \event -> do
        preventDefault event
        setText "Requesting post list..." message
        postList <- call GetPosts
        setText "Got post list..." message
        setText "" posts
        forM_ postList $ \(postid, Title title') -> do
            setText ("Adding: " <> title') message
            li <- select "<li/>"
            a <- select "<a href='#'/>"
            setText title' a
            appendJQuery a li
            appendJQuery li posts
            return ()

            click' a $ \event -> do
                preventDefault event
                setText ("Loading: " <> title') message
                mres <- call $ GetContent postid
                case mres of
                    Nothing -> setText "Post not found" message
                    Just (_, Content content) -> do
                        setText title' h1
                        setText content article
                return ()
        setText "Finished updates posts" message
        return ()

    return ()
