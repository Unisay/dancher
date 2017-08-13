{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Response where

import Lib.Prelude
import Servant (Handler, (:~>)(NT))
import qualified Topic.Repo as TR

type Response = ReaderT Env Handler

newtype Env = Env { getTopicsRepo :: TR.Repo }

responseToHandler :: Env -> Response :~> Handler
responseToHandler env = NT $ \rt -> runReaderT rt env

