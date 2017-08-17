{-# LANGUAGE DataKinds #-}

module Topic.Controller (topicServer) where

import Servant
import Lib.Prelude
import Data.Aeson.WithField
import qualified Topic.Repo as Repo
import Topic.Api
import Topic.Domain
import Response (Response, Env, responseToHandler, dbConnections)

topicServer :: Env -> Server TopicApi
topicServer env = enter (responseToHandler env) serverT

serverT :: ServerT TopicApi Response
serverT = allTopics       -- GET    /topics
     :<|> lookupTopic     -- GET    /topics/:topic-id
     :<|> createTopic     -- POST   /topics
     :<|> deleteTopic     -- DELETE /topics/:topic-id
     :<|> upsertTopic     -- PUT    /topics/:topic-id

identify :: TopicId -> Topic -> WithField "id" TopicId Topic
identify = WithField

allTopics :: Response [TopicEntity]
allTopics = do
  db <- dbConnections <$> ask
  topics <- liftIO $ Repo.all db
  return $ map (uncurry identify) topics

lookupTopic :: TopicId -> Response TopicEntity
lookupTopic topicId = do
  db <- dbConnections <$> ask
  maybeTopic <- liftIO $ Repo.lookup db topicId
  maybe (throwError err404) (return . identify topicId) maybeTopic

upsertTopic :: TopicId -> Topic -> Response TopicEntity
upsertTopic id newTopic = do
  db <- dbConnections <$> ask
  liftIO $ Repo.upsert db id newTopic
  return $ identify id newTopic

createTopic :: Topic -> Response TopicEntity
createTopic newTopic = do
  db <- dbConnections <$> ask
  liftIO $ do
    topicId <- Repo.insert db newTopic
    return $ identify topicId newTopic

deleteTopic :: TopicId -> Response NoContent
deleteTopic id = do
  db <- dbConnections <$> ask
  deleted <- liftIO $ Repo.delete db id
  if deleted
    then return NoContent
    else lift (throwError err404)
