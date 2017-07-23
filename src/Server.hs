{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lib.Prelude hiding (ask)
import Servant
import qualified Topic.Repo as TR

type Response = ReaderT Env Handler

newtype Env = Env { getTopicsRepo :: TR.Repo }

responseToHandler :: Env -> Response :~> Handler
responseToHandler env = Nat $ \rt -> runReaderT rt env
