module App.Routes where

import App.Types (TopicId)
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.?))
import Data.Argonaut.Core as A
import Data.Function (($))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, bind, pure, show, (*>), (<$>), (<*), (<>))
import Pux.Router (end, int, lit, router)

data Route = Topics
           | Topic TopicId
           | NotFound String

derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show (Topics) = "Topics"
  show (Topic id) = "Topic(" <> (show id) <> ")"
  show (NotFound url) = "NotFound(" <> (show url) <> ")"

instance encodeJsonRoute :: EncodeJson Route where
  encodeJson (Topics) = encodeJson $
    A.fromObject (M.fromFoldable [ Tuple "key" (A.fromString "Topics") ])
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
      "Topics" -> pure Topics
      "Topic" -> Topic <$> o .? "topic"
      otherwise -> NotFound <$> o .? "url"

match :: String -> Route
match url = fromMaybe (NotFound url) $
  router url $ Topics <$ end
           <|> Topic <$> (lit "topic" *> int) <* end

toURL :: Route -> String
toURL (Topics) = "/"
toURL (Topic id) = "/topic/" <> (show id)
toURL (NotFound url) = url
