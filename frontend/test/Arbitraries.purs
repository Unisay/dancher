module Test.Arbitraries where

import Prelude
import App.Routes (Route(..))
import App.State (State(..))
import App.Types (Topic(..))
import Control.Monad.Gen (oneOf)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Facebook.Sdk (Status(Unknown), StatusInfo(StatusInfo)) as FB
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

newtype ArbRoute = ArbRoute Route
instance arbitraryRoute :: Arbitrary ArbRoute where
  arbitrary = oneOf $ map ArbRoute <$> pure Home :| [NotFound <$> arbitrary]


newtype ArbTopic = ArbTopic Topic
derive instance newtypeArbTopic :: Newtype ArbTopic _
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
derive newtype instance eqArbState :: Eq ArbState
derive newtype instance showArbState :: Show ArbState
derive newtype instance encodeArbState :: EncodeJson ArbState
derive newtype instance decodeArbState :: DecodeJson ArbState
instance arbitraryState :: Arbitrary ArbState where
  arbitrary = do
    title <- arbitrary
    (ArbRoute route) <- arbitrary
    loaded <- arbitrary
    topics <- map unwrap <$> (arbitrary :: Gen (List ArbTopic))
    expanded <- map unwrap <$> (arbitrary :: Gen (Maybe ArbTopic))
    archived <- map unwrap <$> (arbitrary :: Gen (List ArbTopic))
    menuActive <- arbitrary
    pure $ ArbState $ State { title: title
                            , route: route
                            , loaded: loaded
                            , topics: topics
                            , expanded: expanded
                            , archived: archived
                            , menuActive: menuActive
                            , fbSdk: Nothing
                            , fbAuth: FB.StatusInfo { status: FB.Unknown
                                                    , authResponse: Nothing
                                                    }
                            }
