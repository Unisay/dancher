module App.Backend where

import Prelude
import App.Config (Config)
import App.Types (Topic, Topics(..))
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX, get)

loadTopics :: forall e. Config -> Aff (ajax :: AJAX | e) (Array Topic)
loadTopics config = do
  res <- get (config.api_path <> "topics")
  let (Topics topics) = res.response
  pure topics
