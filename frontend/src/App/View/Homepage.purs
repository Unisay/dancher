module App.View.Homepage (view) where

import App.Events (Event)
import App.State (State(..))
import App.View.TopicList as TopicList
import CSS (fromString, pct, width, (?))
import Data.Function ((#), ($))
import Data.Monoid (mempty)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))
import Control.Bind (discard)


view :: State -> HTML Event
view (State { topics : []}) = do
  div ! className "full-width mdl-progress mdl-js-progress mdl-progress__indeterminate" $ mempty
view s = TopicList.view s
