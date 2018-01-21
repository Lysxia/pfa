module Data.PFA.Internal.Log.Class
  ( Version
  , zeroVersion
  , nextVersion
  , Logging(..)
  ) where

import Data.PFA.Internal.Version

class Logging log where
  newLog :: IO (log a)
  pushLog :: log a -> Version -> a -> IO (log a)
  getLog :: log a -> Version -> IO (Maybe a)
