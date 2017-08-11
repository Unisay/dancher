module App.Events where

import Prelude
import App.Routes (Route)
import App.State (State(..))
import App.Types (Topic)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event = PageView Route
  | AppWaiting
  | AppStarted (Array Topic)
  | GetTopics

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }
foldp AppWaiting s =
  noEffects s
foldp (AppStarted topics) s@(State st) =
  { state: State st { topics = topics }
  , effects: [ (log $ joinWith "; " (map show topics)) *> pure Nothing ]
  }
foldp GetTopics  (State st) =
  noEffects $ State st
