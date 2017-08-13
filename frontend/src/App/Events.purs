module App.Events where

import Prelude

import App.Backend (loadTopics)
import App.Config (config)
import App.Routes (Route)
import App.State (State(..))
import App.Types (Topic)
import Data.Array (delete)
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)

data Event = PageView Route
           | LoadTopics
           | TopicsLoaded (Array Topic)
           | CloseTopic Topic

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }

foldp LoadTopics s =
  onlyEffects s [ Just <$> TopicsLoaded <$> loadTopics config ]

foldp (TopicsLoaded topics) s@(State st) =
  noEffects $ State st { topics = topics }

foldp (CloseTopic topic) (State st) =
  noEffects $ State st { topics = delete topic st.topics, closed = topic : st.closed }
