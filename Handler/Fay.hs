module Handler.Fay where

import Import hiding (Content)
import Yesod.Fay
import Fay.Convert (readFromFay')
import SharedTypes
import qualified Data.Map as Map

onCommand :: CommandHandler App
onCommand render command = do
    $logDebug $ tshow command
    case readFromFay' command of
      Right (AddPost x y r)  -> addPost x y  >>= render r
      Right (GetPosts r)     -> getPosts     >>= render r
      Right (GetContent p r) -> getContent p >>= render r
      Left e                 -> invalidArgs ["Invalid command: " ++ pack e]

addPost :: Title -> Content -> Handler PostId
addPost t c = do
    $logDebug "addPost"
    ref <- fmap postsRef getYesod
    atomicModifyIORef ref $ \m ->
        let newId
                | null m = 1
                | otherwise = fst (Map.findMax m) + 1
         in (insertMap newId (t, c) m, newId)

getPosts :: Handler [(PostId, Title)]
getPosts = do
    ref <- fmap postsRef getYesod
    m <- readIORef ref
    return $ map (\(pid, (title, _)) -> (pid, title))
           $ mapToList m

getContent :: PostId -> Handler (Maybe (Title, Content))
getContent pid = do
    ref <- fmap postsRef getYesod
    m <- readIORef ref
    return $ lookup pid m

deriving instance Num PostId
