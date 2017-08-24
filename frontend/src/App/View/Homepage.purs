module App.View.Homepage (view) where

import App.View.TopicList as TopicList
import App.Events (Event)
import App.State (State(..))
import Control.Bind (discard)
import Data.Function (($))
import Data.List (List(..))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, li, ul, div)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup ((!), text)
import Markup (empty)

view :: State -> HTML Event
view (State { topics : Nil }) =
  div ! id "loading" $ do
  	div ! className "loading1 loading" $ empty
  	div ! className "loading2 loading" $ empty
  	div ! className "loading3 loading" $ empty
  	div ! className "loading4 loading" $ empty
  	div ! className "loading5 loading" $ empty
  	div ! className "loading6 loading" $ empty
  	div ! className "loading7 loading" $ empty
  	div ! className "loading8 loading" $ empty
view s = do
  div ! className "tabs" $
    ul $ do
      li ! className "is-active" $ a (text "Все темы")
      li $ a (text "Избранные темы")
      li $ a (text "Архив тем")
  TopicList.view s
