module Topic.Controller (server) where

import Server
import Servant
import Lib.Prelude
import Topic.Api
import Topic.Domain
import qualified Topic.Repo as Repo
import Control.Monad.Trans.Maybe

server :: Env -> Server TopicApi
server env = enter (appMtoHandler env) serverT

serverT :: ServerT TopicApi AppM
serverT = allTopics    -- GET  /topics
  :<|> lookupTopic     -- GET  /topics/:topic-id
  :<|> createTopic     -- POST /topics
  :<|> upsertTopic     -- PUT  /topics/:topic-id

allTopics :: AppM [Topic]
allTopics = do
  env <- ask
  liftIO $ Repo.all (getTopicsRepo env)

lookupTopic :: TopicId -> AppM Topic
lookupTopic topicId = do
  env <- ask
  let maybeTopic = Repo.lookup (getTopicsRepo env) topicId
  lift $ maybeToExceptT err404 maybeTopic

upsertTopic :: TopicId -> Topic -> AppM Topic
upsertTopic id topic = do
  env <- ask
  let newTopic = topic { getTopicId = id }
  liftIO $ Repo.upsert (getTopicsRepo env) newTopic
  return newTopic

createTopic :: Topic -> AppM Topic
createTopic topic = do
  env <- ask
  id <- liftIO $ Repo.generateId (getTopicsRepo env)
  let newTopic = topic { getTopicId = id }
  liftIO $ Repo.insert (getTopicsRepo env) newTopic
  return newTopic
