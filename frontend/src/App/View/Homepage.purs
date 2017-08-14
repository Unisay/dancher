module App.View.Homepage (view) where

import App.View.TopicList as TopicList
import App.Events (Event)
import App.State (State(..))
import Data.Function (($))
import Data.List (List(..))
import Data.Monoid (mempty)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))


view :: State -> HTML Event
view (State { topics : Nil}) = do
  div ! className "full-width mdl-progress mdl-js-progress mdl-progress__indeterminate" $ mempty
view s = TopicList.view s
