module Main where

import Prelude
import App.Events (AppEffects, Event(..), foldp)
import App.Routes (match)
import App.State (State, init)
import App.View.Layout (view)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (getItem, setItem)
import DOM.WebStorage.Types (Storage)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal (runSignal, (~>))
import Signal.Channel (channel, subscribe)

type WebApp = App (DOMEvent -> Event) Event State
type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  win <- window

  storage <- localStorage win
  loadedState <- loadState storage

  chan <- channel $ InitApp "320125848412942"
  let appStatusSignal = subscribe chan

  -- | Create a signal of URL changes.
  urlSignal <- sampleURL win

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- | Start the app.
  app <- start { initialState: loadedState
               , view: view
               , foldp: foldp
               , inputs: [appStatusSignal, routeSignal]
               }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Persist state to localStorage
  runSignal $ app.state ~> saveState storage

  pure app

loadState :: ∀ fx. Storage -> Eff (dom :: DOM, console :: CONSOLE | fx) State
loadState stor = do
  storedState <- getItem "pux:state" stor
  let loaded = (jsonParser >=> decodeJson) <$> storedState
      defaultState = init "/"
      default = fromMaybe (Right defaultState) loaded
      onError s = log s $> defaultState
  state <- either onError pure default
  pure state

saveState :: ∀ fx. Storage -> State -> Eff (dom :: DOM | fx) Unit
saveState stor st = setItem "pux:state" (show (encodeJson st)) stor
