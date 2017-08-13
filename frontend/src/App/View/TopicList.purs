module App.View.TopicList where

import App.Events (Event(..))
import App.State (State(..))
import App.Types (Topic(..))
import CSS (Size(Size), backgroundColor, em, fromString, margin, minHeight, nil, whitesmoke, width, (?))
import CSS.Common (auto)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Prelude hiding (div)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, button, div, i)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (MarkupM, text, (!), (#!))

view :: State -> HTML Event
view (State st) = div do
    style do
      fromString ".topic-card" ? do
        margin (0.5 #em) nil nil (0.5 #em)
        backgroundColor whitesmoke
        width (Size $ fromString "calc(100% - 1.1em)")
        minHeight auto
    for_ st.topics \t@(Topic topic) -> div ! className "topic-card mdl-card mdl-shadow--4dp mdl-card--border" $ do
      div ! className "mdl-card__title" $ do
        text topic.title
        spacer
        button ! className "mdl-button mdl-js-button mdl-button--icon" #! onClick (const (CloseTopic t)) $
          i ! className "close-topic material-icons" $ text "close"
      -- div ! className "mdl-card__media" $ mempty
      div ! className "mdl-card__supporting-text" $ text (fromMaybe "No description" topic.description)
      div ! className "mdl-card__actions mdl-card__border" $ do
        spacer
        a ! className "mdl-button mdl-button--raised mdl-button--accent mdl-js-button mdl-js-ripple-effect" $ text "Открыть"

spacer :: forall a. MarkupM  a Unit
spacer = div ! className "mdl-layout-spacer" $ mempty
