module Markup where

import App.Events (Event)
import Control.Monad.Free (liftF)
import Prelude (($), unit)
import Pux.DOM.Events (DOMEvent)
import Text.Smolder.Markup (Markup, MarkupM(Empty))

type Part = Markup (DOMEvent -> Event)

-- | https://github.com/bodil/purescript-smolder/pull/33
empty :: ∀ e. Markup e
empty = liftF $ Empty unit
