module Data.Event where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Maybe

import Debug.Trace

newtype Event = Event
  { name :: Maybe String
  , date :: Maybe String
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

