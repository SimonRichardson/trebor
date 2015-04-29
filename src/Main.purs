module Main where

import Database.Mongo.Mongo
import Database.Mongo.ConnectionInfo
import Database.Mongo.Types

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Either

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
  find ["name":> "Amazing"] ["name":>1, "date":>1] col
  liftEff $ traceAny col
