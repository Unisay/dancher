module App.Routes where

import Data.Argonaut.Core as A
import Data.StrMap as M
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.?))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, bind, pure, show, (<$>), (<>), (>>=))
import Pux.Router (end, router)

data Route = Home | NotFound String

derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show (NotFound url) = "NotFound(" <> (show url) <> ")"
  show (Home) = "Home"

instance encodeJsonRoute :: EncodeJson Route where
  encodeJson (NotFound url) =
    encodeJson $ A.fromObject (M.fromFoldable [
        Tuple "key" (A.fromString "NotFound")
      , Tuple "url" (A.fromString url)
      ])
  encodeJson (Home) =
    encodeJson $ A.fromObject (M.fromFoldable [Tuple "key" (A.fromString "Home")])

instance decodeJsonRoute :: DecodeJson Route where
  decodeJson json = do
    o <- decodeJson json
    key <- o .? "key"
    case key of
      "Home" -> pure Home
      otherwise -> NotFound <$> o .? "url"

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url (Home <$ end)

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/"
