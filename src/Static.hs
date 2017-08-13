{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Static (StaticApi, staticApi, staticServer) where

import Servant
import Lib.Prelude

type StaticApi = Get '[PlainText] Text :<|> Raw

staticApi :: Proxy StaticApi
staticApi = Proxy

staticServer :: Server StaticApi
staticServer = throwError err301 { errHeaders = [("Location", "index.html")] }
          :<|> serveDirectoryWebApp "frontend/static"

