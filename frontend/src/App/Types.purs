module Types where

import Data.Show
import Data.Generic

type TopicId = Int

newtype Topic = Topic
  { getTopicId :: TopicId
  , getTopicTitle :: String
  , getTopicDescription :: String
  }

derive instance genericTopic :: Generic Topic

instance showTopic :: Show Topic where show = gShow
