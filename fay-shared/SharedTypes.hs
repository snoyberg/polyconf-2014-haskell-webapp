{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SharedTypes where

import Prelude
import Data.Data
import Language.Fay.Yesod
#ifdef FAY
import FFI
#else
--import Language.Fay.FFI
#endif

newtype PostId = PostId Int
    deriving (Read, Typeable, Data, Show)
data Title = Title Text
    deriving (Read, Typeable, Data, Show)
data Content = Content Text
    deriving (Read, Typeable, Data, Show)

data Command
    = AddPost Title Content (Returns PostId)
    | GetPosts (Returns [(PostId, Title)])
    | GetContent PostId (Returns (Maybe (Title, Content)))
    deriving (Read, Typeable, Data, Show)

