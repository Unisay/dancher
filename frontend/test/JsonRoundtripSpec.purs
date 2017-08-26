module JsonRoundtrip where

import Prelude (class Eq, class Show, discard, show, ($), (<>), (>>>))
import Test.Arbitraries (ArbRoute, ArbState, ArbTopic)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (either)
import Data.Newtype (unwrap)
import Test.QuickCheck (Result(Failed), assertEquals)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Json roundtrip" do
  test "Route" $ quickCheck routeRoundtrip
  test "Topic" $ quickCheck topicRoundtrip
  test "State" $ quickCheck stateRoundtrip

routeRoundtrip :: ArbRoute -> Result
routeRoundtrip = unwrap >>> roundTrip

topicRoundtrip :: ArbTopic -> Result
topicRoundtrip = unwrap >>> roundTrip

stateRoundtrip :: ArbState -> Result
stateRoundtrip = unwrap >>> roundTrip

roundTrip :: ∀ a. Show a => Eq a => EncodeJson a => DecodeJson a => a -> Result
roundTrip a =
  let json = encodeJson a
      fail e = Failed $ "Failed to decode " <> (show json) <> ": " <> e
      verify = assertEquals a
  in either fail verify $ decodeJson json
