module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, GenericFontFamily(GenericFontFamily), backgroundImage, backgroundRepeat, em, fontFamily, fontSize, fromString, marginLeft, padding, pct, rem, repeat, url, width, (?))
import Control.Bind (discard)
import Data.Function ((#), ($))
import Data.Monoid (mempty)
import Data.NonEmpty (singleton)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, p, i, div, h2, nav, section, footer, span)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (!))

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
        h2 ! className "subtitle" $ text "Всегда есть о чём поговорить!"
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

css :: CSS
css = do
  fromString ".full-width" ?
    width (100.0 #pct)
  fromString "body" ? do
    backgroundImage $ url "/img/sayagata.png"
    backgroundRepeat repeat
  fromString ".hero-body" ?
    padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
  fromString ".logo" ? do
    marginLeft (0.5 #em)
    fontFamily ["Chewy"] (singleton (GenericFontFamily $ fromString "cursive"))
    fontSize $ em 2.0
