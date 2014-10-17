module Handler.Fay where

import Import hiding (Content)
import SharedTypes
import qualified Data.Map as Map
import Data.Aeson (eitherDecodeStrict')

postCommandR :: Handler Value
postCommandR = do
    mtext <- lookupPostParam "json"
    bs <- maybe (invalidArgs ["json not provided"]) (return . encodeUtf8) mtext
    command <- either (invalidArgs . return . pack) return
             $ eitherDecodeStrict' bs
    let render :: ToJSON a => Returns a -> a -> Handler Value
        render Returns x = returnJson x
    case command of
      AddPost x y r  -> addPost x y  >>= render r
      GetPosts r     -> getPosts     >>= render r
      GetContent p r -> getContent p >>= render r
      ExpensiveComputation i r -> liftIO (expensiveComputation i) >>= render r

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
