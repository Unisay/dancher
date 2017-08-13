{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lib.Prelude hiding (ask)

import Servant
import Static (staticServer, StaticApi)
import Topic (topicServer, TopicApi)
import Response (Env)

type Api = "api" :> TopicApi :<|> StaticApi

api :: Proxy Api
api = Proxy

server :: Env -> Server Api
server env = topicServer env :<|> staticServer
