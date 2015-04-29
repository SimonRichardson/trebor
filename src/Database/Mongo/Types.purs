module Database.Mongo.Types 
  ( Document(..)
  , Field(..)
  , ObjectId(..)
  , Value(..)
  , Val, val
  , (:=)
  , printBson
  ) where

import Data.Tuple
import Data.Foreign
import Data.String.Regex

type Field = Tuple String Value

type Document = [Field]

data ObjectId = ObjectId String

data Value
  = VString   String
  | VNumber   Number
  | VRegex    Regex
  | VDocument [Tuple String Value]
  | VArray    [Value]
  | VObjectId ObjectId

class Val a where
  val :: a -> Value

instance stringVal :: Val String where
  val = VString

instance numberVal :: Val Number where
  val = VNumber

instance regexVal :: Val Regex where
  val = VRegex

instance documentType :: Val [Tuple String Value] where
  val = VDocument

instance arrayVal :: Val [Value] where
  val = VArray

instance objectIdVal :: Val ObjectId where
  val = VObjectId

infix 0 :=
    
(:=) :: forall a. (Val a) => String -> a -> Tuple String Value
(:=) f v = Tuple f (val v)

printBson :: forall a. Document -> Foreign
printBson = _printBson

foreign import _printBson
  """
  function _printBson(doc){
    return doc.reduce(function(b, a) {
      b[a["value0"]] = a["value1"]["value0"];
      return b;
    }, {});
  }
  """ :: Document -> Foreign