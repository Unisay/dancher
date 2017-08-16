{-# LANGUAGE DeriveGeneric   #-}

module Topic.Domain where

import Lib.Prelude
import Data.Aeson
import Data.Aeson.Types

type TopicId = Int

data Topic = Topic
  { getTopicTitle :: Text
  , getTopicDescription :: Maybe Text
  , getTopicSubtitle :: Text
  , getTopicBody :: Text
  , getTopicQuestions :: [Text]
  , getTopicRefs :: [Text] -- todo: named references + lang
  } deriving (Eq, Show, Generic)

topicJsonOpts :: Options
topicJsonOpts = defaultOptions { fieldLabelModifier = camelCase . stripPrefix
                               , omitNothingFields = True
                               } where camelCase = camelTo2 '_'
                                       stripPrefix = drop 8

instance ToJSON Topic where
  toJSON = genericToJSON topicJsonOpts

instance FromJSON Topic where
  parseJSON = genericParseJSON topicJsonOpts
