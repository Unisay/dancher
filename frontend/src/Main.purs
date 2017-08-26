module Main where

import Prelude

import App.Config (config)
import App.Events (AppEffects, Event(..), foldp)
import App.Routes (match)
import App.State (State(State), init)
import App.View.Layout (view)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (pathname, search)
import DOM.HTML.Types (HISTORY, Window)
import DOM.HTML.Window (localStorage, location)
import DOM.WebStorage.Storage (getItem, setItem)
import DOM.WebStorage.Types (Storage)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Traversable (traverse)
import Pux (CoreEffects, start)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal (runSignal, (~>))
import Signal.Channel (channel, subscribe)

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main :: Eff ClientEffects Unit
main = do
  win <- window
  storage <- localStorage win
  initialState <- initState win storage

  chan <- channel $ InitApp config.fbAppId
  let appStatusSignal = subscribe chan

  -- | Create a signal of URL changes.
  urlSignal <- sampleURL win

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> match >>> PageView

  -- | Start the app.
  app <- start { initialState: initialState
               , view: view
               , foldp: foldp
               , inputs: [appStatusSignal, routeSignal]
               }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Persist state to localStorage
  runSignal $ app.state ~> saveState storage

initState :: ∀ e. Window -> Storage -> Eff (dom :: DOM, console :: CONSOLE | e) State
initState win = loadState >=> maybe (init <$> windowUrl win) overrideRoute
  where overrideRoute (State st) = do
          currentRoute <- match <$> windowUrl win
          pure $ State $ st { route = currentRoute }

loadState :: ∀ e. Storage -> Eff (dom :: DOM, console :: CONSOLE | e) (Maybe State)
loadState stor = do
  maybeRaw <- getItem storageKey stor
  let err s = log ("Failed to parse loaded state becasue of error: " <> s) $> Nothing
  either err pure $ traverse (jsonParser >=> decodeJson) maybeRaw

windowUrl :: ∀ e. Window -> Eff (dom :: DOM, console :: CONSOLE | e) String
windowUrl win = do
  loc <- location win
  path <- pathname loc
  search <- search loc
  pure $ path <> search

saveState :: ∀ fx. Storage -> State -> Eff (dom :: DOM | fx) Unit
saveState stor st = setItem storageKey (show (encodeJson st)) stor

storageKey :: String
storageKey = "dancher:state"
