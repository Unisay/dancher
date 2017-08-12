module App.Events where

import Prelude

import App.Backend (loadTopics)
import App.Config (config)
import App.Routes (Route)
import App.State (State(..))
import App.Types (Topic)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)

data Event = PageView Route
           | LoadTopics
           | TopicsLoaded (Array Topic)

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }

foldp LoadTopics s =
  onlyEffects s [ Just <$> TopicsLoaded <$> loadTopics config ]

foldp (TopicsLoaded topics) s@(State st) =
  { state: State st { topics = topics }
  , effects: [ (log $ joinWith "; " (map show topics)) *> pure Nothing ]
  }
