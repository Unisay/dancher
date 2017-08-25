
module Test.Main where

import Prelude

import App.State (State(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (either)
import Data.Newtype (class Newtype)
import Test.Unit (Test, failure, suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Test.Arbitraries

main = runTest do
  suite "Json roundtrip" do
    test "State" do
      quickCheck stateRoundtrip

roundTrip :: ∀ a e. Eq a => EncodeJson a => DecodeJson a => a -> Boolean
roundTrip a = either (const false) (eq a) $ decodeJson (encodeJson a)

stateRoundtrip :: ∀ e. ArbState -> Boolean
stateRoundtrip = roundTrip
