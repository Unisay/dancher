module Main where

import Prelude

import App.Events (AppEffects, Event(..), foldp)
import App.Routes (match)
import App.State (State, init)
import App.View.Layout (view)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Pux (CoreEffects, App, Config, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))
import Signal.Channel (channel, subscribe)
import Text.Smolder.Markup (Markup, MarkupM)

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  chan <- channel $ InitApp "320125848412942"
  let appStatusSignal = subscribe chan

  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  let v :: ∀ a. State -> Free (MarkupM (DOMEvent -> Event)) a
      v = view

  -- | Start the app.
  let config :: ∀ fx. Config (DOMEvent -> Event) (State -> HTML Event) State fx
      config = { initialState: state
               , view: v
               , foldp: foldp
               , inputs: [appStatusSignal, routeSignal]
               }
  app <- start config

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app

initialState :: State
initialState = init "/"
