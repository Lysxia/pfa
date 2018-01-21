module Data.PFA.Internal.Log.Class where

newtype Version = Version Word deriving (Eq, Ord, Show)

class Logging log where
  newLog :: Int -> IO (log a)
  pushLog :: log a -> Version -> a -> IO (log a)
  getLog :: log a -> Version -> IO (Maybe a)
