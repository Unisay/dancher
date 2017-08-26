module App.View.TopicList where

import App.Events (Event(..))
import App.Routes (Route(..)) as R
import App.Types (Topic(..))
import Data.Foldable (for_)
import Data.List (List)
import Data.Maybe (fromMaybe)
import Markup (Part)
import Prelude hiding (id,div)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, article, div, h2, nav)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (!), (#!))

view :: List Topic -> HTML Event
view topics = for_ topics viewCollapsedTopic

viewCollapsedTopic :: Topic -> Part
viewCollapsedTopic topic @ (Topic { id: id, title: title, description: description }) = do
  div ! className "topic-card box" $
    article ! className "media" $
      div ! className "media-content" $ do
        div ! className "content" $ do
          h2 $ a ! className "topic-title" #! onClick (Navigate (R.Topic id)) $ text title
          text $ fromMaybe "No description" description
        nav ! className "level is-mobile" $ do
          div ! className "level-left" $ div ! className "level-item" $
            a ! className "button is-small" #! onClick (const (ArchiveTopic topic)) $ text "В архив"
          div ! className "level-right" $ div ! className "level-item" $
            a ! className "button is-primary is-small"
              ! href ("#topic/" <> (show id))
              #! onClick (Navigate (R.Topic id)) $ text "Открыть"
