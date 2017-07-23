{-# LANGUAGE DataKinds #-}

module Topic.Controller (server) where

import Server
import Servant
import Lib.Prelude
import Topic.Api
import Topic.Domain
import Data.Aeson.WithField
import qualified Topic.Repo as Repo
import Control.Monad.Trans.Maybe

server :: Env -> Server TopicApi
server env = enter (appMtoHandler env) serverT

serverT :: ServerT TopicApi AppM
serverT = allTopics    -- GET  /topics
  :<|> lookupTopic     -- GET  /topics/:topic-id
  :<|> createTopic     -- POST /topics
  :<|> upsertTopic     -- PUT  /topics/:topic-id

identify :: TopicId -> Topic -> WithField "id" TopicId Topic
identify = WithField

allTopics :: AppM [WithId TopicId Topic]
allTopics = do
  repo <- getTopicsRepo <$> ask
  topics <- liftIO $ Repo.all repo
  return $ map (uncurry identify) topics

lookupTopic :: TopicId -> AppM (WithId TopicId Topic)
lookupTopic topicId = do
  repo <- getTopicsRepo <$> ask
  let maybeTopic = identify topicId <$> Repo.lookup repo topicId
  lift $ maybeToExceptT err404 maybeTopic

upsertTopic :: TopicId -> Topic -> AppM (WithId TopicId Topic)
upsertTopic id newTopic = do
  repo <- getTopicsRepo <$> ask
  liftIO $ Repo.upsert repo id newTopic
  return $ identify id newTopic

createTopic :: Topic -> AppM (WithId TopicId Topic)
createTopic newTopic = do
  repo <- getTopicsRepo <$> ask
  id <- liftIO $ Repo.generateId repo
  liftIO $ Repo.insert repo id newTopic
  return $ identify id newTopic
