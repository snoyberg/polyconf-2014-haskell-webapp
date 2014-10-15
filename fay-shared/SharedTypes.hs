{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SharedTypes where

import Prelude
import Data.Data
import Language.Fay.Yesod

newtype PostId = PostId Int
    deriving (Read, Typeable, Data, Show)
newtype Title = Title Text
    deriving (Read, Typeable, Data, Show)
newtype Content = Content Text
    deriving (Read, Typeable, Data, Show)

data Command
    = AddPost Title Content (Returns PostId)
    | GetPosts (Returns [(PostId, Title)])
    | GetContent PostId (Returns (Maybe (Title, Content)))
    deriving (Read, Typeable, Data, Show)

#ifndef FAY
deriving instance Eq PostId
deriving instance Ord PostId
deriving instance Num PostId
#endif
