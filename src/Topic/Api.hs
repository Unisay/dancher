{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Topic.Api (TopicApi, TopicEntity) where

import Servant
import Topic.Domain
import Data.Aeson.WithField

type TopicEntity = WithId TopicId Topic

type GetTopics =
  "topics" :> Get '[JSON] [TopicEntity]

type CreateTopic =
  "topics"
  :> ReqBody '[JSON] Topic
  :> Post '[JSON] TopicEntity

type GetTopic =
  "topics" :> Capture "topic-id" TopicId
   :> Get '[JSON] TopicEntity

type UpsertTopic =
  "topics" :> Capture "topic-id" TopicId
   :> ReqBody '[JSON] Topic
   :> Put '[JSON] TopicEntity

type TopicApi =
  GetTopics
  :<|> GetTopic
  :<|> CreateTopic
  :<|> UpsertTopic
