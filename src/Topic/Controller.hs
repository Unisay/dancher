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
server env = enter (responseToHandler env) serverT

serverT :: ServerT TopicApi Response
serverT = allTopics    -- GET  /topics
  :<|> lookupTopic     -- GET  /topics/:topic-id
  :<|> createTopic     -- POST /topics
  :<|> upsertTopic     -- PUT  /topics/:topic-id

identify :: TopicId -> Topic -> WithField "id" TopicId Topic
identify = WithField

allTopics :: Response [TopicEntity]
allTopics = do
  repo <- getTopicsRepo <$> ask
  topics <- liftIO $ Repo.all repo
  return $ map (uncurry identify) topics

lookupTopic :: TopicId -> Response TopicEntity
lookupTopic topicId = do
  repo <- getTopicsRepo <$> ask
  let maybeTopic = identify topicId <$> Repo.lookup repo topicId
  lift $ maybeToExceptT err404 maybeTopic

upsertTopic :: TopicId -> Topic -> Response TopicEntity
upsertTopic id newTopic = do
  repo <- getTopicsRepo <$> ask
  liftIO $ Repo.upsert repo id newTopic
  return $ identify id newTopic

createTopic :: Topic -> Response TopicEntity
createTopic newTopic = do
  repo <- getTopicsRepo <$> ask
  liftIO $ do
    id <- Repo.generateId repo
    inserted <- runMaybeT (Repo.insert repo id newTopic)
    let ret = return (identify id newTopic)
    maybe (throwIO err500) (const ret) inserted

