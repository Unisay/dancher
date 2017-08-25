module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.Events (Event(..))
import App.Routes (Route(..))
import App.State (State(..))
import CSS (CSS, GenericFontFamily(GenericFontFamily), backgroundImage, backgroundRepeat, em, fontFamily, fontSize, fromString, marginLeft, padding, pct, rem, repeat, url, width, (?))
import Control.Bind (discard)
import Data.Function (const, (#), ($))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.NonEmpty (singleton)
import Facebook.Sdk (StatusInfo(..), Status(..)) as FB
import Markup (empty)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, div, span, footer, h2, i, nav, p, section)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (!), (#!))

view :: State -> HTML Event
view state @ (State st) = do
  style css
  nav ! className "navbar" $ do
    div ! className "navbar-brand" $ do
      a ! className "navbar logo" $ text "Dancher"
      div ! className "navbar-burger" #! onClick (const MenuToggle) $ do
        span $ empty
        span $ empty
        span $ empty
    div ! className ("navbar-menu" <> if st.menuActive then " is-active" else "") $ do
      div ! className "navbar-end" $ do
        div ! className "navbar-item" $
          case st.fbAuth of
          FB.StatusInfo { status: FB.Connected } ->
            a ! className "button" #! onClick (const FacebookLogoutRequest) $ do
              span ! className "icon is-small" $
                i ! className "fa fa-sign-out" $ empty
              span $ text "Выйти"
          otherwise ->
            a ! className "button" #! onClick (const FacebookLoginRequest) $ do
              span ! className "icon is-small" $
                i ! className "fa fa-sign-in" $ empty
              span $ text "Войти"

  section ! className "hero is-primary" $ do
    div ! className "hero-body" $ do
      div ! className "container" $ do
        h2 ! className "subtitle" $ text "Всегда есть о чём поговорить!"
  case st.route of
    (Home) -> Homepage.view state
    (Topic id) -> Homepage.view $ State st { expanded = Just id }
    (NotFound url) -> NotFound.view state

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
            i ! className "fa fa-github" $ empty

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
