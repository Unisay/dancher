module App.View.TopicList where

import App.Events (Event(..))
import App.State (State(..))
import App.Types (Topic(..))
import CSS (Size(Size), backgroundColor, em, fromString, margin, minHeight, nil, whitesmoke, width, (?))
import CSS.Common (auto)
import CSS.Stylesheet (CSS)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, p, button, div, h6, i)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup, text, (!), (#!))
import Prelude hiding (div)

type Part = Markup (DOMEvent -> Event)

view :: State -> HTML Event
view (State { expanded: Just t@(Topic topic) }) = div do
  style topicCardStyle
  card (ShrinkTopic t) topic.title (body topic) (cardButton (ShrinkTopic t) "Закрыть") where
    body { situation: s } = do
      h6 ! className "title" $ text "Ситуация"
      p $ text s

view (State st) = div do
  style topicCardStyle
  for_ st.topics topicCard where
    topicCard t@(Topic topic) =
      let closeEvent = ArchiveTopic t
          openEvent = ExpandTopic t
          cardBody = text $ fromMaybe "No description" topic.description
          cardFooter = cardButton openEvent "Открыть"
      in card closeEvent topic.title cardBody cardFooter

card :: Event -> String -> Part -> Part -> Part
card closeEvent title body footer =
  div ! className "topic-card mdl-card mdl-shadow--4dp mdl-card--border" $ do
    div ! className "mdl-card__title" $ do
      text title
      spacer
      button #! onClick (const closeEvent) ! className "mdl-button mdl-js-button mdl-button--icon" $
        i ! className "close-topic material-icons" $ text "close"
    div ! className "mdl-card__supporting-text" $ body
    div ! className "mdl-card__actions mdl-card__border" $ footer

spacer :: Part
spacer = div ! className "mdl-layout-spacer" $ mempty

cardButton :: Event -> String -> Part
cardButton ev caption =
  a ! className "mdl-button mdl-button--raised mdl-button--accent mdl-js-button mdl-js-ripple-effect"
    #! onClick (const ev) $ text caption

topicCardStyle :: CSS
topicCardStyle = do
  fromString ".topic-card" ? do
    margin (0.5 #em) nil nil (0.5 #em)
    backgroundColor whitesmoke
    width (Size $ fromString "calc(100% - 1.1em)")
    minHeight auto
