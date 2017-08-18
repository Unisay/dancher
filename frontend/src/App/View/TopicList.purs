module App.View.TopicList where

import App.Events (Event(..))
import App.State (State(..))
import App.Types (Topic(..))
import CSS (alignContent, alignItems, fromString, margin, marginTop, padding, paddingLeft, paddingRight, rem, (**), (?))
import CSS.Common (center)
import CSS.Stylesheet (CSS)
import Control (onlyIf)
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Prelude hiding (id,div)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, article, div, figure, h2, h4, i, nav, p, span, ol, li)
import Text.Smolder.HTML.Attributes (className, href, id)
import Text.Smolder.Markup (Markup, text, (!), (#!))

type Part = Markup (DOMEvent -> Event)

view :: State -> HTML Event
view (State { expanded: Just topic }) =
  div ! className "topic-cards container" $ expandedTopicCard topic

view (State st) =
  div ! className "topic-cards container" $ for_ st.topics shrankTopicCard

shrankTopicCard :: Topic -> Part
shrankTopicCard topic@(Topic { title: title, description: description }) =
  let header = topicHeader title (ExpandTopic topic)
      body = text $ fromMaybe "No description" description
      primaryAction = cardButton (ExpandTopic topic) "Открыть"
      secondaryAction = a ! className "button is-small" #! onClick (const (ArchiveTopic topic)) $ text "В архив"
  in card header body primaryAction secondaryAction

expandedTopicCard :: Topic -> Part
expandedTopicCard topic@(Topic t) =
  let header = topicHeader t.title (ShrinkTopic topic)
      body = do
              h4 ! id "situation" ! className "subtitle is-4" $ text t.subtitle
              p $ text t.body
              onlyIf (Nil /= t.refs) do
                h4 ! id "refs" $ text "Ссылки по теме"
                ol $ for_ t.refs \reference ->
                  li ! className "reference" $ a ! href reference $ text reference
              h4 ! id "questions" ! className "subtitle is-4" $ text "Вопросы"
              for_ t.questions \question -> do
                article ! className "question media" $ do
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
  fromString ".question" ?
    alignItems center
  (fromString ".content" ** fromString ".media-left") ?
    margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
