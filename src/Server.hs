{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( server
  , Env (..)
  ) where

import Lib.Prelude hiding (ask)
import Servant
import Api (TopicApi)
import Domain
import qualified Topics
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

type AppM = ReaderT Env Handler

newtype Env = Env { getTopicsRepo :: Topics.Repo }

server :: Env -> Server TopicApi
server env = enter (appMtoHandler env) serverT

serverT :: ServerT TopicApi AppM
serverT = allTopics           -- GET  /topics
  :<|> lookupTopicHandler     -- GET  /topics/:topic-id
  :<|> createTopicHandler     -- POST /topics
  :<|> upsertTopicHandler     -- PUT  /topics/:topic-id

appMtoHandler :: Env -> AppM :~> Handler
appMtoHandler env = Nat $ \rt -> runReaderT rt env

allTopics :: AppM [Topic]
allTopics = do
  env <- ask
  liftIO $ Topics.all (getTopicsRepo env)

lookupTopicHandler :: TopicId -> AppM Topic
lookupTopicHandler topicId = do
  env <- ask
  let maybeTopic = Topics.lookup (getTopicsRepo env) topicId
  lift $ maybeToExceptT err404 maybeTopic

upsertTopicHandler :: TopicId -> Topic -> AppM Topic
upsertTopicHandler id topic = do
  env <- ask
  let newTopic = topic { getTopicId = id }
  liftIO $ Topics.upsert (getTopicsRepo env) newTopic
  return newTopic

createTopicHandler :: Topic -> AppM Topic
createTopicHandler topic = do
  env <- ask
  id <- liftIO $ Topics.generateId (getTopicsRepo env)
  let newTopic = topic { getTopicId = id }
  liftIO $ Topics.insert (getTopicsRepo env) newTopic
  return newTopic

