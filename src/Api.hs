{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Domain
import Servant

type TopicApi = "topics" :> Get '[JSON] [Topic]
           :<|> "topics" :> Capture "topic-id" TopicId :> Get '[JSON] Topic
--           :<|> "topics" :> Post '[JSON] Topic
--           :<|> "topics" :> Capture "topic-id" TopicId :> Put '[JSON] Topic
