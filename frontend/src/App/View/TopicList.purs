module App.View.TopicList where

import App.Events (Event(..))
import App.State (State(..))
import App.Types (Topic(..))
import CSS (fromString, margin, marginTop, padding, paddingBottom, paddingLeft, paddingRight, rem, (**), (?))
import CSS.Stylesheet (CSS)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Prelude hiding (id,div)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, article, div, figure, h2, h4, i, nav, p, span)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (Markup, text, (!), (#!))

type Part = Markup (DOMEvent -> Event)

view :: State -> HTML Event
view (State { expanded: Just topic }) =
  div ! className "topic-cards container" $ expandedTopicCard topic

view (State st) =
  div ! className "topic-cards container" $ for_ st.topics shrankTopicCard

shrankTopicCard :: Topic -> Part
shrankTopicCard topic@(Topic { title: title, description: description }) =
  let header = topicHeader title (ArchiveTopic topic)
      body = text $ fromMaybe "No description" description
      primaryAction = cardButton (ExpandTopic topic) "Обсудить"
      secondaryAction = a ! className "button is-small" #! onClick (const (ArchiveTopic topic)) $ text "В архив"
  in card header body primaryAction secondaryAction

expandedTopicCard :: Topic -> Part
expandedTopicCard topic@(Topic { title: title, situation: situation, questions: questions }) =
  let header = topicHeader title (ShrinkTopic topic)
      body = do
              h4 ! id "situation" ! className "subtitle is-4" $ text "Ситуация"
              p $ text situation
              h4 ! id "questions" ! className "subtitle is-4" $ text "Вопросы"
              for_ questions \question -> do
                article ! className "media" $ do
                  figure ! className "media-left" $
                    span ! className "icon is-large" $ i ! className "fa fa-question-circle-o" $ mempty
                  div ! className "media-content" $
                    div ! className "content" $ text question
      primaryAction = cardButton (ShrinkTopic topic) "Закрыть"
      secondaryAction = a ! className "button is-small" #! onClick (const (ArchiveTopic topic)) $ text "В архив"
  in card header body primaryAction secondaryAction

topicHeader :: String -> Event -> Part
topicHeader title event = a ! className "topic-title" #! onClick (const event) $ text title

card :: Part -> Part -> Part -> Part -> Part
card title body primaryAction secondaryAction = do
  style topicCardStyle
  div ! className "topic-card box" $
    article ! className "media" $
      div ! className "media-content" $ do
        div ! className "content" $ do
          h2 title
          body
        nav ! className "level is-mobile" $ do
          div ! className "level-left" $ div ! className "level-item" $ secondaryAction
          div ! className "level-right" $ div ! className "level-item" $ primaryAction

cardButton :: Event -> String -> Part
cardButton ev caption =
  a ! className "button is-primary is-small" #! onClick (const ev) $ text caption

topicCardStyle :: CSS
topicCardStyle = do
  fromString ".topic-cards" ? do
    paddingLeft (rem 0.75)
    paddingRight (rem 0.75)
  fromString ".topic-card" ? do
    padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
  fromString "#questions" ?
    marginTop (rem 3.0)
  (fromString ".content" ** fromString ".media-left") ?
    margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
