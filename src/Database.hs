module Database where

import Lib.Prelude (IO, ByteString)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)

newtype Config = Config { databaseConnUri :: ByteString }

type Connections = Pool Connection

connectionPool :: Config -> IO Connections
connectionPool (Config uri) = createPool (connectPostgreSQL uri) close 1 10 20
