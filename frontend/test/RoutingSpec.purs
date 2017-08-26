module RoutingSpec where

import App.Routes (Route(..)) as R
import App.Routes (match, toURL)
import Prelude (discard, ($))
import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)

spec :: ∀ e. TestSuite e
spec = do
  suite "Route <-> URL roundtrip" do
    test "Topics"   $ roundtrip R.Topics
    test "Topic"    $ roundtrip (R.Topic 42)
    test "NotFound" $ roundtrip (R.NotFound "/blah")
  suite "URL matching" do
    test "Topics"   $ R.Topics `equal` match "/"
    test "Topic"    $ R.Topic 1 `equal` match "/topic/1"
    test "NotFound" $ R.NotFound "/bar" `equal` match "/bar"

roundtrip :: ∀ e. R.Route -> Test e
roundtrip route = route `equal` (match (toURL route))
