{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Response where

import Lib.Prelude
import Servant (Handler, (:~>)(NT))
import qualified Database as Db (Connections)

type Response = ReaderT Env Handler

newtype Env = Env { dbConnections :: Db.Connections }

responseToHandler :: Env -> Response :~> Handler
responseToHandler env = NT $ \rt -> runReaderT rt env

