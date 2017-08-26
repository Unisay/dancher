
module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Prelude (Unit, discard)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import RoutingSpec as RoutingSpec
import JsonRoundtrip as JsonRoundtrip

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 , random     :: RANDOM
                 | e
                 ) Unit
main = runTest do
  RoutingSpec.spec
  JsonRoundtrip.spec
