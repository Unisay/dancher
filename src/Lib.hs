{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib ( application ) where

import Lib.Prelude
import Network.Wai
import Servant (serve)
import Server
import Api

application :: Application
application = serve topicApi server where
  topicApi :: Proxy TopicApi
  topicApi = Proxy
