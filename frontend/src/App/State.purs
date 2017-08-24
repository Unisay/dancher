module App.State where

import Prelude
import App.Config (config)
import App.Routes (Route, match, toURL)
import App.Types (Topic)
import Control.Plus (empty)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Facebook.Sdk (Sdk, StatusInfo(StatusInfo), Status(..)) as FB
import Facebook.Sdk (Status(..))

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , topics :: List Topic
  , expanded :: Maybe Topic
  , archived :: List Topic
  , menuActive :: Boolean
  , fbSdk :: Maybe FB.Sdk
  , fbAuth :: FB.StatusInfo
  }

derive instance newtypeState :: Newtype State _

-- | TODO: test roundtrip

instance decodeJsonState :: DecodeJson State where
  decodeJson json = do
    o          <- decodeJson json
    title      <- o .? "title"
    url        <- o .? "route"
    loaded     <- o .? "loaded"
    topics     <- o .? "topics"
    epanded :: Maybe Topic   <- o .? "expanded"
    archived   <- o .? "archived"
    expanded   <- o .? "expanded"
    menuActive <- o .? "menuActive"
    pure $ State { title: title
                 , route: match url
                 , loaded: loaded
                 , topics: topics
                 , expanded: expanded
                 , archived: archived
                 , menuActive: menuActive
                 , fbSdk: Nothing
                 , fbAuth: FB.StatusInfo { status: FB.Unknown
                                         , authResponse: Nothing
                                         }
                 }

instance encodeJsonState :: EncodeJson State where
  encodeJson (State st) =
       "title"      := st.title
    ~> "route"      := toURL st.route
    ~> "loaded"     := st.loaded
    ~> "topics"     := st.topics
    ~> "expanded"   := st.expanded
    ~> "archived"   := st.archived
    ~> "menuActive" := st.menuActive
    ~> jsonEmptyObject

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , topics: empty
  , expanded: empty
  , menuActive: false
  , archived: empty
  , fbSdk: empty
  , fbAuth: FB.StatusInfo { status: Unknown, authResponse: Nothing }
  }
