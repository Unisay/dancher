module App.Events where

import Prelude
import App.Backend (loadTopics)
import App.Config (config)
import App.Routes (Route)
import App.State (State(..))
import App.Types (Topic, TopicId)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.List (List(..), delete)
import Data.Maybe (Maybe(..), maybe)
import Facebook.Sdk (Sdk, StatusInfo(..), Status(..), AppId, defaultConfig, init, login, loginStatus, logout) as FB
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)

data Event = PageView Route
           | InitApp FB.AppId
           | TopicsLoaded (List Topic)
           | ExpandTopic TopicId
           | ShrinkTopic TopicId
           | ArchiveTopic Topic
           | FacebookSdkInitialized FB.Sdk
           | FacebookAuthRequest
           | FacebookLoginRequest
           | FacebookLogoutRequest
           | UserAuth FB.StatusInfo
           | MenuToggle

type AppEffects fx = (console :: CONSOLE, ajax :: AJAX | fx)

facebookEffect :: ∀ e. State -> (FB.Sdk -> Aff e Event) -> Aff e (Maybe Event)
facebookEffect (State st) f = maybe (throwError $ error "FB sdk isn't initialized") (f >>> map Just) st.fbSdk

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (FacebookSdkInitialized sdk) (State st) =
  { state: State st { fbSdk = Just sdk }
  , effects: [ pure $ Just FacebookAuthRequest ]
  }

foldp FacebookAuthRequest state =
  onlyEffects state [ facebookEffect state (map UserAuth <<< FB.loginStatus) ]

foldp FacebookLoginRequest state =
  onlyEffects state [ facebookEffect state (map UserAuth <<< FB.login) ]

foldp FacebookLogoutRequest state =
  onlyEffects state [ facebookEffect state (map UserAuth <<< FB.logout) ]

-- | Facebook Auth Status transition: * -> Connected
foldp (UserAuth fbAuth)
      (State st @ { fbAuth: (FB.StatusInfo { status: FB.Connected, authResponse: (Just _)}) }) =
  { state: State st { fbAuth = fbAuth, topics = Nil }
  , effects: [ Just <$> TopicsLoaded <$> loadTopics config ]
  }

-- | Facebook Auth Status transition: Connected -> *
foldp (UserAuth fbAuth @ (FB.StatusInfo { status: FB.Connected, authResponse: auth })) (State st) =
  { state: State st { fbAuth = fbAuth, topics = Nil }
  , effects: [ Just <$> TopicsLoaded <$> loadTopics config ]
  }

-- | Facebook Auth Status transition: * -> *
foldp (UserAuth fbAuth) (State st) =
  noEffects $ State st { fbAuth = fbAuth }


foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }

foldp (InitApp fbAppId) s =
  onlyEffects s [ Just <$> TopicsLoaded <$> loadTopics config
                , Just <$> FacebookSdkInitialized <$> FB.init (FB.defaultConfig fbAppId)
                ]

foldp (TopicsLoaded topics) s@(State st) =
  noEffects $ State st { topics = topics }

foldp (ExpandTopic topicId) (State st) =
  noEffects $ State st { expanded = Just topicId }

foldp (ShrinkTopic topic) (State st) =
  noEffects $ State st { expanded = Nothing }

foldp (ArchiveTopic topic) (State st) =
  noEffects $ State st { topics = delete topic st.topics
                       , expanded = Nothing
                       }

foldp MenuToggle (State st) =
  noEffects $ State st { menuActive = not st.menuActive }
