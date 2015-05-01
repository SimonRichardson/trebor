module Database.Redis.Redis where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Either
import Data.Foreign
import Data.Function (Fn4(), runFn4)

import Database.Redis.ConnectionInfo

-- | The effect type for DB request made with Mongo
foreign import data DB :: !

foreign import data Client :: *

type AffClient e = Aff (db :: DB | e) Client

-- | Makes a connection to the database.
connect :: forall e. ConnectionInfo -> AffClient e
connect = makeAff' <<< connect'

-- | Run a request directly without using 'Aff'
connect' :: forall e
  .  ConnectionInfo
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Client -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
connect' info eb cb = runFn4 _connect (dialUri info) ignoreCancel eb cb

-- | Always ignore the cancel.
ignoreCancel :: forall e a. a -> Canceler (db :: DB | e)
ignoreCancel c = Canceler \err -> makeAff (\eb cb -> runFn4 _ignoreCancel c err eb cb)

-- | foreign imports
foreign import _connect
  """
  function _connect(uri, canceler, errback, callback) {
    var Redis = require('ioredis'),
        client = new Redis({lazyConnect: true});

    client.options.enableReadyCheck = true;
    client.parseOptions(uri);
    client.connect();
    client.once('ready', function(err, _) {
        (err ? errback(err) : callback(client))();
    });
    return canceler(client);
  }
  """ :: forall e. Fn4 
                   Uri
                   (Client -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Client -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _ignoreCancel
  """
  function _ignoreCancel(any, cancelError, errback, callback) {
    return function() {
        callback(false);
    };
  }
  """ :: forall e a. Fn4 
                     a
                     Error
                     (Error -> Eff (db :: DB | e) Unit)
                     (Boolean -> Eff (db :: DB | e) Unit)
                     (Eff (db :: DB | e) Unit)