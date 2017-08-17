module Main where

import Lib.Prelude
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Options.Applicative
import Data.Semigroup ((<>))
import Data.ByteString.Char8 (pack)
import Servant (serve)
import Response
import Server (api, server)
import qualified Database as Db (Config(Config), connectionPool)

data Opts = Opts  { port :: Int
                  , databaseConfig :: Db.Config
                  }

portOption :: Parser Int
portOption = option auto (  long "port"
                         <> short 'p'
                         <> help "TCP port to bind"
                         <> showDefault
                         <> value 8080
                         <> metavar "INT"
                         )

databaseUri :: Parser Db.Config
databaseUri = Db.Config . pack <$> op where
  op = strOption (  long "database-uri"
                 <> short 'd'
                 <> metavar "DATABASE_URI"
                 <> help "Database connection URI"
                 )

options :: Parser Opts
options = Opts <$> portOption
               <*> databaseUri

main :: IO ()
main = execParser opts >>= withOpts
  where
    opts = info ( options <**> helper )
                ( fullDesc
               <> progDesc "Dancher REST backend"
               <> header "Dancher REST backend server" )

withOpts :: Opts -> IO ()
withOpts (Opts tcpPort dbConfig) = do
  putStrLn ("Server listens at http://localhost:" <> show tcpPort :: Text)
  db <- Db.connectionPool dbConfig
  run tcpPort $ serverCors $ application (Env db)

application :: Env -> Application
application env = serve api (server env)

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
