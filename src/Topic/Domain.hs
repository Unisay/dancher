{-# LANGUAGE DeriveGeneric   #-}

module Topic.Domain where

import Lib.Prelude
import Data.Aeson
import Data.Aeson.Types

type TopicId = Int

data Topic = Topic
  { getTopicTitle :: Text
  , getTopicDescription :: Text
  } deriving (Eq, Show, Generic)

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
