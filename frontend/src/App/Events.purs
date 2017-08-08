module App.Events where

import App.Routes (Route)
import App.State (State(..))
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import App.Types (Topic)

data Event = PageView Route
  | AppWaiting
  | AppStarted (Array Topic)
  | GetTopics

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }
foldp AppWaiting (State st) =
  noEffects $ State st
foldp (AppStarted topics) (State st) =
  noEffects $ State st { topics = topics }
foldp GetTopics  (State st) =
  noEffects $ State st
