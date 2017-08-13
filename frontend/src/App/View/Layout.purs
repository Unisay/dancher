module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, GenericFontFamily(GenericFontFamily), backgroundImage, backgroundRepeat, em, pct, width, fontFamily, fontSize, fromString, repeat, url, (?))
import Control.Bind (discard)
import Data.Function ((#), ($))
import Data.Monoid (mempty)
import Data.NonEmpty (singleton)
import Prelude (Unit)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div, header, span, nav, a)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (MarkupM, text, (!))

view :: State -> HTML Event
view s@(State st) =
  div ! className "mdl-layout mdl-js-layout mdl-layout--fixed-header" $ do
    style css
    header ! className "layout-transparent mdl-layout__header" $ do
      div ! className "mdl-layout__header-row" $ do
        span ! className "logo mdl-layout-title" $ text "Dancher"
        div ! className "mdl-layout-spacer" $ mempty
        nav ! className "mdl-navigation" $ githubLink
    div ! className  "mdl-layout__drawer" $ do
      span ! className "mdl-layout-title" $ do
        text "Dancher"
        nav ! className "mdl-navigation" $ githubLink
    div ! className "mdl-layout__content" $ do -- TODO: use custom tag "main"
      case st.route of
        (Home) -> Homepage.view s
        (NotFound url) -> NotFound.view s

css :: CSS
css = do
  fromString ".full-width" ? width (100.0 #pct)
  fromString ".mdl-layout" ? do
    backgroundImage $ url "/img/sayagata.png"
    backgroundRepeat repeat
  fromString ".logo" ? do
    fontFamily ["Chewy"] (singleton (GenericFontFamily $ fromString "cursive"))
    fontSize $ em 2.0

githubLink :: ∀ m. MarkupM m Unit
githubLink = a ! className "mdl-navigation__link" ! href "https://github.com/unisay/dancher/" $ text "GitHub"
