module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Newtype (class Newtype)
import Types (Topic)

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , topics :: Array Topic
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , topics: []
  }
