module App.View.Homepage (view) where

import App.View.Topic as Topic
import App.View.TopicList as TopicList
import App.Events (Event)
import App.State (State(..))
import App.Types (Topic(..))
import CSS.Stylesheet (CSS)
import CSS (fromString, padding, paddingLeft, paddingRight, rem, (?))
import Data.List (List(..), find)
import Data.Maybe (maybe)
import Markup (empty)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, li, ul, div)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup ((!), text)
import Prelude hiding (id,div)

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
view state @ (State st) = do
  style topicCardStyle
  div ! className "tabs" $ do
    ul $ do
      li ! className "is-active" $ a (text "Все темы")
      li $ a (text "Избранные темы")
      li $ a (text "Архив тем")
  let topicById id = find (\(Topic t) -> t.id == id) st.topics
      expandedTopic = st.expanded >>= topicById
      topicsList = TopicList.view st.topics
  div ! className "topic-cards container" $
    maybe topicsList Topic.view expandedTopic

topicCardStyle :: CSS
topicCardStyle = do
  fromString ".topic-cards" ? do
    paddingLeft (rem 0.75)
    paddingRight (rem 0.75)
  fromString ".topic-card" ? do
    padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
