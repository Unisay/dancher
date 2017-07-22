{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lib.Prelude hiding (ask)
import Servant
import qualified Topic.Repo as TR

type AppM = ReaderT Env Handler

newtype Env = Env { getTopicsRepo :: TR.Repo }

appMtoHandler :: Env -> AppM :~> Handler
appMtoHandler env = Nat $ \rt -> runReaderT rt env
