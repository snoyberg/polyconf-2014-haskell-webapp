{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Home where

import Prelude hiding (show)
import Fay.FFI
import Language.Fay.Yesod
import SharedTypes
import Lib

main :: Fay ()
main = do
    title   <- select "#title"
    content <- select "#content"
    addPost <- select "#add-post"
    message <- select "#message"
    update  <- select "#update"
    posts   <- select "#posts"
    h1      <- select "#display > h1"
    article <- select "#display > article"

    flip click addPost $ \event -> do
        preventDefault event
        setText "Adding post..." message
        title' <- getVal title
        content' <- getVal content
        call (AddPost (Title title') (Content content')) $ \_ -> do
            setText "New post added" message
            return ()

    flip click update $ \event -> do
        preventDefault event
        setText "Requesting post list..." message
        call GetPosts $ \postList -> do
            setText "Got post list..." message
            setText "" posts
            forM_ postList $ \(postid, Title title') -> do
                setText ("Adding: " <> title') message
                li <- select "<li/>"
                a <- select "<a href='#'/>"
                setText title' a
                append a li
                append li posts
                return ()

                flip click a $ \event -> do
                    preventDefault event
                    setText ("Loading: " <> title') message
                    call (GetContent postid) $ \mres -> do
                        case mres of
                            Nothing -> setText "Post not found" message
                            Just (_, Content content) -> do
                                setText title' h1
                                setText content article
                        return ()
                    return ()
            setText "Finished updates posts" message
            return ()

    return ()
