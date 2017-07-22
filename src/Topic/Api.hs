{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Topic.Api (TopicApi, topicApi) where

import Servant
import Topic.Domain

type GetTopics =
  "topics" :> Get '[JSON] [Topic]

type CreateTopic =
  "topics"
  :> ReqBody '[JSON] Topic
  :> Post '[JSON] Topic

type GetTopic =
  "topics" :> Capture "topic-id" TopicId
   :> Get '[JSON] Topic

type UpsertTopic =
  "topics" :> Capture "topic-id" TopicId
   :> ReqBody '[JSON] Topic
   :> Put '[JSON] Topic

type TopicApi =
  GetTopics
  :<|> GetTopic
  :<|> CreateTopic
  :<|> UpsertTopic

topicApi :: Proxy TopicApi
topicApi = Proxy
