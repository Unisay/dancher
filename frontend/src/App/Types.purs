module App.Types where

import Prelude
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode ((.??))
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Bifunctor (lmap)
import Data.Foreign (F, ForeignError(..))
import Data.Generic (class Generic, gEq, gShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..), fromResponse)
import Unsafe.Coerce (unsafeCoerce)

type TopicId = Int

newtype Topic = Topic
  { id :: TopicId
  , title :: String
  , description :: Maybe String
  , subtitle :: String
  , body :: String
  , refs :: List String
  , questions :: List String
  }

newtype Topics = Topics (List Topic)

derive instance genericTopic :: Generic Topic

derive instance genericTopics :: Generic Topics

instance eqTopic :: Eq Topic where eq = gEq

instance showTopic :: Show Topic where show = gShow

instance decodeJsonTopic :: DecodeJson Topic where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    description <- obj .? "description"
    subtitle <- obj .? "subtitle"
    body <- obj .? "body"
    refs <- obj .? "refs"
    questions <- obj .? "questions"
    pure $ Topic { id, title, description, subtitle, body, refs, questions }

instance decodeJsonTopics :: DecodeJson Topics where
  decodeJson json = Topics <$> decodeJson json

instance encodeJsonTopic :: EncodeJson Topic where
  encodeJson (Topic t) =
       "id" := t.id
    ~> "title" := t.title
    ~> "subtitle" := t.subtitle
    ~> "description" := t.description
    ~> "body" := t.body
    ~> "refs" := t.refs
    ~> "questions" := t.questions
    ~> jsonEmptyObject

instance requestableTopic :: Requestable Topic where
  toRequest json = Tuple (Just applicationJSON) (unsafeCoerce (show json))

-- https://stackoverflow.com/questions/42927827/purescript-reuse-argonaut-json-decoding-for-affjax-respondeable
decodeJsonResponse :: forall a. DecodeJson a => Json -> F a
decodeJsonResponse = ExceptT <<< pure <<< lmap (pure <<< ForeignError) <<< decodeJson

instance respondableTopic :: Respondable Topic where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = decodeJsonResponse <=< fromResponse

-- TODO: consider deriving the instance without using newtype
instance respondableTopics :: Respondable Topics where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = decodeJsonResponse <=< fromResponse
