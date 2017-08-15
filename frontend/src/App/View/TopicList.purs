module App.View.TopicList where

import App.Events (Event(..))
import App.State (State(..))
import App.Types (Topic(..))
import CSS (fromString, paddingLeft, paddingRight, rem, (?))
import CSS.Stylesheet (CSS)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, article, div, h2, h6, i, li, nav, ol, p, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup, text, (!), (#!))
import Prelude hiding (div)

type Part = Markup (DOMEvent -> Event)

view :: State -> HTML Event
view (State { expanded: Just topic }) =
  expandedTopicCard topic

view (State st) =
  div ! className "topic-cards" $
    for_ st.topics shrankTopicCard

shrankTopicCard :: Topic -> Part
shrankTopicCard topic@(Topic { title: title, description: description }) =
  let header = topicHeader title (ArchiveTopic topic)
      body = text $ fromMaybe "No description" description
      footer = cardButton (ExpandTopic topic) "Обсудить"
  in card header body footer

expandedTopicCard :: Topic -> Part
expandedTopicCard topic@(Topic { title: title, situation: situation, questions: questions }) =
  let header = topicHeader title (ShrinkTopic topic)
      body = do
              h6 ! className "title" $ text "Ситуация:"
              p $ text situation
              h6 ! className "title" $ text "Вопросы:"
              ol ! className "mdl-list" $ for_ questions \question -> do
                li ! className "mdl-list__item" $ do
                  span ! className "mdl-list__item-primary-content" $ do
                    i ! className "material-icons mdl-list__item-avatar" $ text "help outline"
                    text question
      footer = nav ! className "level" $
                div ! className "level-right" $
                  div ! className "level-item" $
                    cardButton (ShrinkTopic topic) "Закрыть"
  in card header body footer

topicHeader :: String -> Event -> Part
topicHeader title event = a ! className "topic-title" #! onClick (const event) $ text title

card :: Part -> Part -> Part -> Part
card title body footer = do
  style topicCardStyle
  div ! className "topic-card box" $
    article ! className "media" $
      div ! className "media-content" $ do
        h2 title
        p body
        footer

cardButton :: Event -> String -> Part
cardButton ev caption =
  a ! className "button is-primary is-small" #! onClick (const ev) $ text caption

topicCardStyle :: CSS
topicCardStyle = do
  fromString ".topic-cards" ? do
    paddingLeft (0.75 #rem)
    paddingRight (0.75 #rem)
