module App.Routes where

import Data.Argonaut.Core as A
import Data.StrMap as M
import App.Types (TopicId)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.?))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, bind, pure, show, (<$>), (<>))
import Pux.Router (end, router)

data Route = Home
           | Topic TopicId
           | NotFound String

derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show (Home) = "Home"
  show (Topic id) = "Topic(" <> (show id) <> ")"
  show (NotFound url) = "NotFound(" <> (show url) <> ")"

instance encodeJsonRoute :: EncodeJson Route where
  encodeJson (Home) = encodeJson $
    A.fromObject (M.fromFoldable [ Tuple "key" (A.fromString "Home") ])
  encodeJson (Topic id) = encodeJson $
    A.fromObject (M.fromFoldable [ Tuple "key" (A.fromString "Topic")
                                 , Tuple "topic" (A.fromString (show id))
                                 ])
  encodeJson (NotFound url) = encodeJson $
    A.fromObject (M.fromFoldable [ Tuple "key" (A.fromString "NotFound")
                                 , Tuple "url" (A.fromString url)
                                 ])

instance decodeJsonRoute :: DecodeJson Route where
  decodeJson json = do
    o <- decodeJson json
    key <- o .? "key"
    case key of
      "Home" -> pure Home
      "Topic" -> Topic <$> o .? "topic"
      otherwise -> NotFound <$> o .? "url"

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url (Home <$ end)

toURL :: Route -> String
toURL (Home) = "/"
toURL (Topic id) = "/topic/" <> (show id)
toURL (NotFound url) = url
