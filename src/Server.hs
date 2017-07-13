{-# LANGUAGE DataKinds #-}

module Server
  ( server
  ) where

import Lib.Prelude
import Servant
import Api (TopicApi)
import Domain
import qualified Topics

server :: Server TopicApi
server =
  return Topics.all           -- GET  /topics
  :<|> lookupTopicHandler     -- GET  /topics/:topic-id
  :<|> createTopicHandler     -- POST /topics
  :<|> upsertTopicHandler     -- PUT  /topics/:topic-id

lookupTopicHandler :: TopicId -> Handler Topic
lookupTopicHandler = maybe (throwError err404) return . Topics.lookup

upsertTopicHandler :: TopicId -> Topic -> Handler Topic
upsertTopicHandler id topic = do
  let newTopic = topic { getTopicId = id }
  _ <- liftIO $ Topics.upsert newTopic
  return newTopic

createTopicHandler :: Topic -> Handler Topic
createTopicHandler = undefined
