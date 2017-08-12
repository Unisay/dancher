module App.View.TopicList where

import Prelude hiding (div)
import App.Events (Event)
import App.State (State(..))
import App.Types (Topic(..))
import Data.Foldable (for_)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, span, ul, li)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.HTML.Attributes (className)

view :: State -> HTML Event
view (State st) = div do
  ul ! className "mdl-list" $ do
    for_ st.topics \(Topic topic) ->
      li ! className "mdl-list__item mdl-list__item--two-line" $ do
        span ! className "mdl-list__item-primary-content" $ do
          span $ text topic.title
          span ! className "mdl-list__item-sub-title" $ text topic.description
