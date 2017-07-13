{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Lib.Prelude
import Data.Time (UTCTime)
import Data.Aeson
import Data.Aeson.Types

type TopicId = Int

data Topic = Topic
  { getTopicId :: TopicId
  , getTopicTitle :: Text
  , getTopicDescription :: Text
  , getTopicCreated :: UTCTime
  } deriving (Show, Generic)

instance Eq Topic where
  (==) t1 t2 = getTopicId t1 == getTopicId t2

topicJsonOpts :: Options
topicJsonOpts = defaultOptions {
    fieldLabelModifier = camelCase . stripPrefix
  } where
  camelCase = camelTo2 '_'
  stripPrefix = drop 8

instance ToJSON Topic where
  toJSON = genericToJSON topicJsonOpts

instance FromJSON Topic where
  parseJSON = genericParseJSON topicJsonOpts
