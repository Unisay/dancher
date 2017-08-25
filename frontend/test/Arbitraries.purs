module Test.Arbitraries where

import Prelude
import App.Routes (Route(..)) as R
import App.State (State(..))
import App.Types (Topic(Topic))
import Control.Monad.Gen (oneOf)
import Data.Argonaut (class EncodeJson)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Facebook.Sdk (Status(Unknown), StatusInfo(StatusInfo)) as FB
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

newtype ArbRoute = ArbRoute R.Route
derive instance newtypeArbRoute :: Newtype ArbRoute _
instance arbitraryRoute :: Arbitrary ArbRoute where
  arbitrary = oneOf $ map ArbRoute <$> pure R.Topics :| [R.NotFound <$> pure "url"]


newtype ArbTopic = ArbTopic Topic
derive instance newtypeArbTopic :: Newtype ArbTopic _
derive newtype instance encodeJsonArbTopic :: EncodeJson ArbTopic
derive newtype instance showArbTopic :: Show ArbTopic
instance arbitraryTopic :: Arbitrary ArbTopic where
  arbitrary = do
    id <- arbitrary
    title <- arbitrary
    descr <- arbitrary
    subtitle <- arbitrary
    body <- arbitrary
    refs <- arbitrary
    questions <- arbitrary
    pure $ ArbTopic $ Topic { id: id
                            , title: title
                            , description: descr
                            , subtitle: subtitle
                            , body: body
                            , refs: refs
                            , questions: questions
                            }

newtype ArbState = ArbState State
derive instance newtypeArbState :: Newtype ArbState _
instance arbitraryState :: Arbitrary ArbState where
  arbitrary = do
    title <- arbitrary
    (ArbRoute route) <- arbitrary
    topics <- map unwrap <$> (arbitrary :: Gen (List ArbTopic))
    archived <- map unwrap <$> (arbitrary :: Gen (List ArbTopic))
    menuActive <- arbitrary
    pure $ ArbState $ State { title: title
                            , route: route
                            , topics: topics
                            , archived: archived
                            , menuActive: menuActive
                            , fbSdk: Nothing
                            , fbAuth: FB.StatusInfo { status: FB.Unknown -- TODO arbtrary
                                                    , authResponse: Nothing
                                                    }
                            }
