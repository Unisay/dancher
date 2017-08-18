module Control where

import Data.Monoid (class Monoid, mempty)

onlyIf :: ∀ a. Monoid a => Boolean -> a -> a
onlyIf b p = if b then p else mempty
