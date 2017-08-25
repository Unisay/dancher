module App.View.Topic where

import App.Events (Event(..))
import App.Types (Topic(..))
import CSS (alignItems, fromString, margin, marginTop, padding, rem, (**), (?))
import CSS.Common (center)
import CSS.Stylesheet (CSS)
import Data.Foldable (for_)
import Data.List (List(Nil))
import Markup (Part, empty)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, article, div, figure, h2, h4, i, nav, p, span, ol, li)
import Text.Smolder.HTML.Attributes (className, href, id)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (id,div)

view :: Topic -> HTML Event
view topic@(Topic t) = do
  style topicCardStyle
  div ! className "topic-card box" $
    article ! className "media" $
      div ! className "media-content" $ do
        div ! className "content" $ do
          h2 $ topicHeader t.title (ShrinkTopic t.id)
          do
            h4 ! id "situation" ! className "subtitle is-4" $ text t.subtitle
            p $ text t.body
            if (Nil /= t.refs) then do
              h4 ! id "refs" $ text "Ссылки по теме"
              ol $ for_ t.refs \reference ->
                li ! className "reference" $ a ! href reference $ text reference
              else empty
            h4 ! id "questions" ! className "subtitle is-4" $ text "Вопросы"
            for_ t.questions \question -> do
              article ! className "question media" $ do
                figure ! className "media-left" $
                  span ! className "icon is-large" $ i ! className "fa fa-question-circle-o" $ empty
                div ! className "media-content" $
                  div ! className "content" $ text question
        nav ! className "level is-mobile" $ do
          div ! className "level-left" $ div ! className "level-item" $
            a ! className "button is-small" #! onClick (const (ArchiveTopic topic)) $ text "В архив"
          div ! className "level-right" $ div ! className "level-item" $
            cardButton (ShrinkTopic t.id) "Закрыть"

topicHeader :: String -> Event -> Part
topicHeader title event = a ! className "topic-title" #! onClick (const event) $ text title

cardButton :: Event -> String -> Part
cardButton ev caption =
  a ! className "button is-primary is-small" #! onClick (const ev) $ text caption

topicCardStyle :: CSS
topicCardStyle = do
  fromString ".topic-card" ? do
    padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
  fromString "#questions" ?
    marginTop (rem 3.0)
  fromString ".question" ?
    alignItems center
  (fromString ".content" ** fromString ".media-left") ?
    margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
