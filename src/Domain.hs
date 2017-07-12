{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Lib.Prelude
import Data.Time (UTCTime)
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)

type TopicId = Int

data Topic = Topic
  { getTopicId :: TopicId
  , getTopicTitle :: Text
  , getTopicDescription :: Text
  , getTopicCreated :: UTCTime
  } deriving (Show, Generic)

instance Eq Topic where
  (==) t1 t2 = getTopicId t1 == getTopicId t2

instance ToJSON Topic where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = fmap toLower . drop 8  }
