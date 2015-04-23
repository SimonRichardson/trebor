module Main where


import Data.Argonaut (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Event
import Data.Maybe

import Debug.Trace

event :: Event
event = Event
  { name : Just "Cool Event"
  , date : Nothing
  }

main = do
  print $ "raw event is: " ++ show event
  print $ "encoded event is: " ++ printJson (encodeJson event)
