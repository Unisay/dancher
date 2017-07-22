module Main where

import Lib.Prelude
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Server
import Topic

main :: IO ()
main = do
  putStrLn ("Server listens at http://localhost:8081" :: Text)
  repo <- newTopicRepo
  run 8081 $ application (Env repo)

application :: Env -> Application
application env = serve topicApi (server env)
