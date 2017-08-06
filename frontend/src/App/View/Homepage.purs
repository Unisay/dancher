module App.View.Homepage where

import App.Events (Event)
import App.State (State)
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), text)

view :: State -> HTML Event
view s =
  div do
    h1 $ text "Dancher"
    a ! className "github" ! href "https://github.com/unisay/dancher/" $ text "GitHub"
