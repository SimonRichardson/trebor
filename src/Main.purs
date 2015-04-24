module Main where


import Data.Argonaut (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Event
import Data.Maybe
import Data.Date

import Debug.Trace

event :: Event
event = Event
  { name : Just "Cool Event"
  , date : fromStringStrict "2015-04-24T12:00:00.000Z"
  }

main = do
  print $ "raw event is: " ++ show event
  print $ "encoded event is: " ++ printJson (encodeJson event)
