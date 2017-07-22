module Topic
  ( TopicApi
  , topicApi
  , server
  , newTopicRepo
  ) where

import Topic.Api
import Topic.Controller (server)
import Topic.Repo (newTopicRepo)
