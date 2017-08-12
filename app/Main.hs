module Main where

import Lib.Prelude
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Options.Applicative
import Data.Semigroup ((<>))
import Servant (serve)
import Server
import Topic

newtype Opts = Opts  { port :: Int }

options :: Parser Opts
options = Opts <$> option auto (  long "port"
                               <> short 'p'
                               <> help "TCP port to bind"
                               <> showDefault
                               <> value 8080
                               <> metavar "INT"
                               )

main :: IO ()
main = execParser opts >>= withOpts
  where
    opts = info (options <**> helper)
                ( fullDesc
               <> progDesc "Dancher REST backend"
               <> header "Dancher REST backend server" )

withOpts :: Opts -> IO ()
withOpts (Opts tcpPort) = do
  putStrLn ("Server listens at http://localhost:" <> show tcpPort :: Text)
  repo <- newTopicRepo
  run tcpPort $ serverCors $ application (Env repo)

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
