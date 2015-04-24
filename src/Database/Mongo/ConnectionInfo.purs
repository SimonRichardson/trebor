module Database.Mongo.ConnectionInfo where

-- | The type for Mongo connection options
type ConnectionInfo =
  { uri    :: URI
  }

defaultOptions :: ConnectionInfo
defaultOptions =
  { uri    : "mongodb://127.0.0.1:27017/test"
  }

-- | Type alias for URI connections to aid readability of types.
type URI = String

dialUri :: ConnectionInfo -> URI
dialUri info = info.uri
