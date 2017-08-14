module App.Events where

import Prelude
import App.Backend (loadTopics)
import App.Config (config)
import App.Routes (Route)
import App.State (State(..))
import App.Types (Topic)
import Data.List (List, delete)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)

data Event = PageView Route
           | LoadTopics
           | TopicsLoaded (List Topic)
           | ExpandTopic Topic
           | ShrinkTopic Topic
           | ArchiveTopic Topic

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }

foldp LoadTopics s =
  onlyEffects s [ Just <$> TopicsLoaded <$> loadTopics config ]

foldp (TopicsLoaded topics) s@(State st) =
  noEffects $ State st { topics = topics }

foldp (ExpandTopic topic) (State st) =
  noEffects $ State st { expanded = Just topic }

foldp (ShrinkTopic topic) (State st) =
  noEffects $ State st { expanded = Nothing }

foldp (ArchiveTopic topic) (State st) =
  noEffects $ State st { topics = delete topic st.topics }
