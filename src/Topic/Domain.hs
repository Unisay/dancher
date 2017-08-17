{-# LANGUAGE DeriveGeneric   #-}

module Topic.Domain where

import Lib.Prelude
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple (ResultError(ConversionFailed))
import Database.PostgreSQL.Simple.FromField (fromField, returnError, FromField)
import Database.PostgreSQL.Simple.ToField (toField, ToField)

type TopicId = Int64

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

instance FromField Topic where
  fromField field mdata = do
     value <- fromField field mdata
     let errorOrTopic = parseEither parseJSON value
     either (returnError ConversionFailed field) return errorOrTopic

instance ToField Topic where
  toField = toField . encode
