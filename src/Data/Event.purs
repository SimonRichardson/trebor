module Data.Event where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Core (Json(), JString(), toString)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson, decodeMaybe)
import Data.Date
import Data.Either
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)

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

-- | Encoding for date json

foreign import toISOString
  """
  function toISOString(date){
    return date.toISOString();
  }
  """ :: Date -> String


instance encodeJsonDate :: EncodeJson Date where
  encodeJson = encodeJson <<< toISOString

-- | Decoding for date json

foreign import fromJString
  """
  function fromJString(str) {
    return str;
  }
  """ :: JString -> String

instance decodeJsonDate :: DecodeJson Date where
  decodeJson json = maybe (Left "Couldn't decode.") Right $ do
    str <- toString json
    fromStringStrict $ fromJString str
  
