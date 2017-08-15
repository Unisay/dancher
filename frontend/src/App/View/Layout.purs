module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import CSS (CSS, GenericFontFamily(GenericFontFamily), backgroundImage, backgroundRepeat, em, fontFamily, fontSize, fromString, marginLeft, pct, repeat, url, width, (?))
import Control.Bind (discard)
import Data.Function ((#), ($))
import Data.Monoid (mempty)
import Data.NonEmpty (singleton)
import Prelude (Unit)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, p, i, div, h2, nav, section, footer, span)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (MarkupM, text, (!))

view :: State -> HTML Event
view s@(State st) = do
  style css
  nav ! className "navbar" $ do
    div ! className "navbar-brand" $ do
      a ! className "navbar logo" $ text "Dancher"
      div ! className "navbar-burger" $ do
        span mempty
        span mempty
        span mempty
  section ! className "hero is-primary" $ do
    div ! className "hero-body" $ do
      div ! className "container" $ do
        h2 ! className "subtitle" $ text "Нам есть о чём поговорить"
  case st.route of
    (Home) -> Homepage.view s
    (NotFound url) -> NotFound.view s

  footer ! className "footer" $
    div ! className "container" $
      div ! className "content has-text-centered" $ do
        p $ do
          text "The source code is licensed "
          a ! href "https://www.gnu.org/licenses/gpl-3.0.en.html" $ text "GPLv3"
        p $ do
          text "The website content is licensed "
          a ! href "http://creativecommons.org/licenses/by-nc-sa/4.0/" $ text "CC ANS 4.0"
        p $
          a ! className "icon" ! href "https://github.com/unisay/dancher/" $
            i ! className "fa fa-github" $ mempty


  --   section ! className "navbar-section" $ mempty
  --   section ! className "navbar-section navbar-center" $
  --   section ! className "navbar-section" $ githubLink
  -- div ! className "panel" $ do
  --   -- div ! className "panel-header" $ do
  --   --   div ! className "panel-title" $ text "Comments"
  --   div ! className "panel-nav" $ do
  --     ul ! className "tab tab-block" $ do
  --       li ! className "tab-item active" $ a ! href "#" $ text "Новые темы"
  --       li ! className "tab-item" $ a ! href "#" $ text "Обсуждённые"
  --   div ! className "panel-body" $

  --   div ! className "panel-footer" $
  --     text "Yuriy Lazarev (c)"

css :: CSS
css = do
  fromString ".full-width" ? width (100.0 #pct)
  fromString "body" ? do
    backgroundImage $ url "/img/sayagata.png"
    backgroundRepeat repeat
  fromString ".logo" ? do
    marginLeft (0.5 #em)
    fontFamily ["Chewy"] (singleton (GenericFontFamily $ fromString "cursive"))
    fontSize $ em 2.0
