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
  { title     :: Text
  , shortText :: Text
  , fullText  :: Text
  , questions :: [Text]
  } deriving (Eq, Show, Generic)

topicJsonOpts :: Options
topicJsonOpts = defaultOptions { fieldLabelModifier = camelTo2 '_'
                               , omitNothingFields = True
                               }

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
