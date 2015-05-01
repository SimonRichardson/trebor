module Main where

import Database.Redis.Redis
import Database.Redis.ConnectionInfo

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Argonaut (printJson)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Event
import Data.Maybe
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
  liftEff $ traceAny database
