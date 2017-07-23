{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Topic.Api (TopicApi, topicApi) where

import Servant
import Topic.Domain
import Data.Aeson.WithField

type GetTopics =
  "topics" :> Get '[JSON] [WithId TopicId Topic]

type CreateTopic =
  "topics"
  :> ReqBody '[JSON] Topic
  :> Post '[JSON] (WithId TopicId Topic)

type GetTopic =
  "topics" :> Capture "topic-id" TopicId
   :> Get '[JSON] (WithId TopicId Topic)

type UpsertTopic =
  "topics" :> Capture "topic-id" TopicId
   :> ReqBody '[JSON] Topic
   :> Put '[JSON] (WithId TopicId Topic)

type TopicApi =
  GetTopics
  :<|> GetTopic
  :<|> CreateTopic
  :<|> UpsertTopic

topicApi :: Proxy TopicApi
topicApi = Proxy
