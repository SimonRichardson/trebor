module Data.Event where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Date
import Data.Time (Milliseconds())
import Data.Either
import Data.Maybe

import Debug.Trace

newtype Event = Event
  { name :: Maybe String
  , date :: Maybe Date
  }

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    date <- obj .? "date"
    pure $ Event
      { name : name
      , date : date
      }

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event e)
    =  "name" := e.name
    ~> "date" := e.date
    ~> jsonEmptyObject

instance showEvent :: Show Event where
  show (Event e) = "Event " ++
    "{ name: " ++ show e.name ++
    ", date: " ++ show e.date ++
    "}"

-- Move to somewhere else!

instance encodeJsonDate :: EncodeJson Date where
  encodeJson = encodeJson <<< toEpochMilliseconds

instance encodeJsonMilliseconds :: EncodeJson Milliseconds where
  encodeJson = fromMilliseconds

instance decodeJsonDate :: DecodeJson Date where
  decodeJson x = do
    v <- toMilliseconds x
    case fromEpochMilliseconds v of
      Just y -> Right y
      Nothing -> Left "Not a Date."

instance decodeJsonMilliseconds :: DecodeJson Milliseconds where
  decodeJson = toMilliseconds

foreign import fromMilliseconds
  """
  function fromMilliseconds(m) {
    return m;
  }
  """ :: Milliseconds -> Json

toMilliseconds :: Json -> Either String Milliseconds
toMilliseconds x = Right $ _toMilliseconds x

foreign import _toMilliseconds
  """
  function _toMilliseconds(x) {
    return x
  }
  """ :: Json -> Milliseconds
