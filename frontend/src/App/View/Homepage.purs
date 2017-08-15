module App.View.Homepage (view) where

import App.View.TopicList as TopicList
import App.Events (Event)
import App.State (State(..))
import Control.Bind (discard)
import Data.Function (($))
import Data.List (List(..))
import Data.Monoid (mempty)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, li, ul, div)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup ((!), text)

view :: State -> HTML Event
view (State { topics : Nil }) =
  div ! id "loading" $ do
  	div ! className "loading1 loading" $ mempty
  	div ! className "loading2 loading" $ mempty
  	div ! className "loading3 loading" $ mempty
  	div ! className "loading4 loading" $ mempty
  	div ! className "loading5 loading" $ mempty
  	div ! className "loading6 loading" $ mempty
  	div ! className "loading7 loading" $ mempty
  	div ! className "loading8 loading" $ mempty
view s = do
  div ! className "tabs" $
    ul $ do
      li ! className "is-active" $ a (text "Новые темы")
      li $ a (text "Архив")
  TopicList.view s
