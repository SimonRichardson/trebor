module Database.Mongo.Mongo where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Either
import Data.Function (Fn2(), runFn2, Fn3(), runFn3, Fn4(), runFn4, Fn5(), runFn5)

import Database.Mongo.ConnectionInfo
import Database.Mongo.Types

-- | The effect type for DB request made with Mongo
foreign import data DB :: !

foreign import data Client :: *
foreign import data Database :: *
foreign import data Collection :: *

type AffDatabase e = Aff (db :: DB | e) Database
type AffCollection e = Aff (db :: DB | e) Collection

-- | Makes a connection to the database.
connect :: forall e. ConnectionInfo -> AffDatabase e
connect = makeAff' <<< connect'

connectWithUri :: forall e. URI -> AffDatabase e
connectWithUri u = connect $ defaultOptions { uri = u }

-- | Get the collection
collection :: forall e. String -> Database -> AffCollection e
collection a b = makeAff' (collection' a b)

-- | Run a request directly without using 'Aff'
connect' :: forall e
  .  ConnectionInfo
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Database -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
connect' info eb cb = runFn4 _connect info ignoreCancel eb cb

collection' :: forall e
  .  String
  -> Database
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Collection -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
collection' name d eb cb = runFn5 _collection name d ignoreCancel eb cb

-- | Find in the collection
find :: forall e a. Document -> Document -> Collection -> Aff (db :: DB | e) a
find = _find

-- | Always ignore the cancel.
ignoreCancel :: forall e a. a -> Canceler (db :: DB | e)
ignoreCancel c = Canceler \err -> makeAff (\eb cb -> runFn4 _ignoreCancel c err eb cb)


-- | foreign imports
foreign import _connect
  """
  function _connect(info, canceler, errback, callback) {
    var client = require('mongodb').MongoClient;
    client.connect(info.uri, function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(client);
  }
  """ :: forall e. Fn4 
                   ConnectionInfo
                   (Client -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Database -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _collection
  """
  function _collection(name, db, canceler, errback, callback) {
    db.collection(name, function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(db);
  }
  """ :: forall e. Fn5 
                   String 
                   Database
                   (Database -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Collection -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _find
  """
  function _find(selector) {
    return function(hint) {
        return function(collection) {
            return function(success, error) {
                success({});
            };
        };
    };
  }
  """ :: forall e a. Document -> Document -> Collection -> Aff (db :: DB | e) a

foreign import _ignoreCancel
  """
  function _ignoreCancel(any, cancelError, errback, callback) {
    return function() {
        callback(false);
    };
  }
  """ :: forall e a. Fn4 a
                     Error
                     (Error -> Eff (db :: DB | e) Unit)
                     (Boolean -> Eff (db :: DB | e) Unit)
                     (Eff (db :: DB | e) Unit)
