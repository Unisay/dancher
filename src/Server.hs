{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (server) where

import Lib.Prelude
import Data.Time.Calendar
import Servant
import Data.Time
import Api (TopicApi)
import Domain

topics :: [Topic]
topics =
  [ Topic 1
      "Topic title 1"
      "Topic description 1"
      (UTCTime (fromGregorian 2016 1 1) 1)
  , Topic 2
      "Topic title 2"
      "Topic description 2"
      (UTCTime (fromGregorian 2017 1 1) 2)
  ]

lookupTopic :: TopicId -> Maybe Topic
lookupTopic id = find ((== id) . getTopicId) topics

server :: Server TopicApi
server = return topics
{-    :<|> return lookupTopicHandler
  where lookupTopicHandler :: TopicId -> Handler Topic
        lookupTopicHandler = undefined -- return . lookupTopic-}

