module App.View.Layout where

import App.Events (Event(..))
import App.Routes (Route(..))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, GenericFontFamily(GenericFontFamily), backgroundImage, backgroundColor, backgroundRepeat, em, fontFamily, fontSize, fromString, marginLeft, padding, pct, rem, rgba, repeat, url, width, (?))
import Control.Bind (discard)
import Data.Function (const, (#), ($))
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (mempty, (<>))
import Data.NonEmpty (singleton)
import Facebook.Sdk as FB
import Markup (empty)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, button, div, footer, h2, i, nav, p, section, span)
import Text.Smolder.HTML.Attributes (className, disabled, href)
import Text.Smolder.Markup (attribute, text, (!), (#!))

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
          case st.fbUserInfo of
          Just (FB.UserInfo { name: (FB.UserName name)}) ->
            div ! className "dropdown is-hoverable" $ do
              div ! className "dropdown-trigger" $
                button ! className "button"
                       ! attribute "aria-haspopup" "true"
                       ! attribute "aria-controls" "dropdown-menu" $ do
                  span $ text name
                  span ! className "icon is-small" $
                    i ! className "fa fa-angle-down"
                      ! attribute "aria-hidden" "true" $ empty
              div ! className "dropdown-menu" ! attribute "role" "menu" $
                div ! className "dropdown-content" $
                  a ! className "dropdown-item" #! onClick (const FacebookLogoutRequest) $ do
                    span ! className "icon is-small" $ i ! className "fa fa-sign-out" $ empty
                    text "Выйти"
          otherwise ->
            a ! className "button"
              ! (if (isNothing st.fbSdk) then disabled "disabled" else mempty)
              #! onClick (const FacebookLoginRequest) $ do
              span ! className "icon is-small" $
                i ! className "fa fa-sign-in" $ empty
              span $ text "Войти"

  section ! className "hero is-primary" $ do
    div ! className "hero-body" $ do
      div ! className "container" $ do
        h2 ! className "subtitle" $ text "Всегда есть о чём поговорить!"
  case st.route of
    (NotFound url) -> NotFound.view state
    otherwise -> Homepage.view state

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
  fromString ".footer" ?
    backgroundColor (rgba 0 0 0 0.0)
