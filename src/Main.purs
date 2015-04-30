module Main where

import Database.Mongo.Mongo
import Database.Mongo.ConnectionInfo
import Database.Mongo.Types

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Argonaut (printJson)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Event
import Data.String.Regex

import Debug.Trace

foreign import traceAny
  """
  function traceAny(a){
    return function () {
      console.log(a);
      return {};
    };
  }
  """ :: forall e a. a -> Eff (trace :: Trace | e) Unit

main = launchAff $ do
  
  Right database <- attempt $ connect $ defaultOptions
  col <- collection "events" database
  res <- findOne ["name" := (regex "Amazing" noFlags)] ["name" := 1] col

  liftEff $ case decodeEvent res of
    Left err -> traceAny err
    Right x -> traceAny x

  where
    decodeEvent :: Json -> (Either String Event)
    decodeEvent = decodeJson
