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
server = return Topics.all :<|> lookupTopicHandler

lookupTopicHandler :: TopicId -> Handler Topic
lookupTopicHandler = maybe (throwError err404) return . Topics.lookup
