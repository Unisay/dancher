
module Test.Main where

import Test.Arbitraries
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (either)
import Data.Newtype (unwrap)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (<>), (>>>))
import Test.QuickCheck (Result(Failed), assertEquals)
import Test.Unit (suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 , random     :: RANDOM
                 | e
                 ) Unit
main = runTest do
  suite "Json roundtrip" do
    test "Route" $ quickCheck routeRoundtrip
    test "Topic" $ quickCheck topicRoundtrip
    test "State" $ quickCheck stateRoundtrip

roundTrip :: ∀ a. Show a => Eq a => EncodeJson a => DecodeJson a => a -> Result
roundTrip a =
  let json = encodeJson a
      fail e = Failed $ "Failed to decode " <> (show json) <> ": " <> e
      verify = assertEquals a
  in either fail verify $ decodeJson json

routeRoundtrip :: ArbRoute -> Result
routeRoundtrip = unwrap >>> roundTrip

topicRoundtrip :: ArbTopic -> Result
topicRoundtrip = unwrap >>> roundTrip

stateRoundtrip :: ArbState -> Result
stateRoundtrip = unwrap >>> roundTrip
