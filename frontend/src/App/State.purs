module App.State where

import Prelude
import App.Config (config)
import App.Routes (Route, match)
import App.Types (Topic, TopicId)
import Control.Plus (empty)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Facebook.Sdk (Sdk, Status(..), StatusInfo(StatusInfo)) as FB

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , topics :: List Topic
  , expanded :: Maybe TopicId
  , archived :: List Topic
  , menuActive :: Boolean
  , fbSdk :: Maybe FB.Sdk
  , fbAuth :: FB.StatusInfo
  }

derive instance newtypeState :: Newtype State _

instance eqState :: Eq State where
  eq (State s) (State s') =
    s.title == s'.title
    && s.route == s'.route
    && s.loaded == s'.loaded
    && s.topics == s'.topics
    && s.expanded == s'.expanded
    && s.archived == s'.archived
    && s.menuActive == s'.menuActive

instance showState :: Show State where
  show = show <<< encodeJson

instance decodeJsonState :: DecodeJson State where
  decodeJson json = do
    o <- decodeJson json
    title <- o .? "title"
    route <- o .? "route"
    loaded <- o .? "loaded"
    topics <- o .? "topics"
    archived <- o .? "archived"
    expanded <- o .? "expanded"
    menuActive <- o .? "menuActive"
    pure $ State { title: title
                 , route: route
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
    ~> "route"      := st.route
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
  , fbAuth: FB.StatusInfo { status: FB.Unknown, authResponse: Nothing }
  }
