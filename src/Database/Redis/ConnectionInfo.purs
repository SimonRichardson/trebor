module Database.Redis.ConnectionInfo where

import Data.Maybe

-- | The type for Redis connection options
type ConnectionInfo =
  { host     :: Host
  , port     :: Maybe Port
  , family   :: Maybe Family
  , user     :: Maybe User
  , password :: Maybe Password
  }

-- | Standard default options for connecting to redis.
defaultOptions :: ConnectionInfo
defaultOptions =
  { host     : "127.0.0.1"
  , port     : Just 6379
  , family   : Just "4"
  , user     : Nothing
  , password : Nothing
  }

-- | Type alias for URI connections to aid readability of types.
type Uri      = String
type Host     = String
type Port     = Number
type Family   = String
type User     = String
type Password = String

dialUri :: ConnectionInfo -> Uri
dialUri info = "redis://" ++ 
    auth info ++ 
    uri info ++
    port info ++
    family info
  
  where
    auth :: ConnectionInfo -> String
    auth info = (maybe "" (\x -> showUser x) info.user) ++ 
                (maybe ":@" (\x -> ":" ++ showPassword x) info.password) ++ 
                "@"
    uri :: ConnectionInfo -> String
    uri info = showHost info.host
    port :: ConnectionInfo -> String
    port info = maybe "" (\x -> ":" ++ showPort x) info.port
    family :: ConnectionInfo -> String
    family info = maybe "" (\x -> "/" ++ showFamily x) info.family

showHost :: Host -> String
showHost h = h

showPort :: Port -> String
showPort p = show p

showFamily :: Family -> String
showFamily f = f

showUser :: User -> String
showUser u = u

showPassword :: Password -> String
showPassword p = p