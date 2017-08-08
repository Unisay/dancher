module Main where

import Lib.Prelude
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant (serve)
import Server
import Topic

main :: IO ()
main = do
  putStrLn ("Server listens at http://localhost:8081" :: Text)
  repo <- newTopicRepo
  run 8081 $ serverCors $ application (Env repo)

application :: Env -> Application
application env = serve topicApi (server env)

serverCors :: Middleware
serverCors = cors $ const (Just serverCorsPolicy)

serverCorsPolicy :: CorsResourcePolicy
serverCorsPolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing -- gives you /* TODO: verify origin
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
    , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }
