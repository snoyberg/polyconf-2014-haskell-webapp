{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module SharedTypes where

import Prelude
import Data.Data
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)

newtype PostId = PostId Int
    deriving (Read, Typeable, Data, Show, Eq, Ord, Num, ToJSON, FromJSON)
newtype Title = Title Text
    deriving (Generic, Read, Typeable, Data, Show, ToJSON, FromJSON)
newtype Content = Content Text
    deriving (Generic, Read, Typeable, Data, Show, ToJSON, FromJSON)

data Returns a = Returns
    deriving (Generic)
instance ToJSON (Returns a)
instance FromJSON (Returns a)

data Command
    = AddPost Title Content (Returns PostId)
    | GetPosts (Returns [(PostId, Title)])
    | GetContent PostId (Returns (Maybe (Title, Content)))
    {-
    deriving (Generic)

instance ToJSON Command
instance FromJSON Command
-}

deriveJSON defaultOptions ''Command
