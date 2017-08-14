module App.State where

import App.Config (config)
import App.Routes (Route, match)
import App.Types (Topic)
import Control.Plus (empty)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , topics :: List Topic
  , expanded :: Maybe Topic
  , archived :: List Topic
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , topics: empty
  , expanded: empty
  , archived: empty
  }
