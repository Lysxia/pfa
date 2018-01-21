module Data.PFA.Internal.Log.Vector where

import Data.Traversable
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU

import Data.PFA.Internal.Log.Class
import Data.PFA.Internal.Version

type Size = Int

-- | Invariant: the two vectors have the same length (capacity)
-- and the size is less than or equal to the capacity.
data Log a = Log !Size !VersionVector !(MV.IOVector a)

newLog_ :: Int -> IO (Log a)
newLog_ n =
  Log 0 <$> newVV n <*> MV.new n

pushLog_ :: Log a -> Version -> a -> IO (Log a)
pushLog_ (Log size vs@(VersionVector vs_) as) v a = do
  let push vs as = do
        unsafeWriteVV vs size v :: IO ()
        MV.unsafeWrite as size a
        return (Log (size + 1) vs as)
  if size == lengthVV vs then do
    Log _ vs'@(VersionVector vs_') as' <- newLog (size * 2)
    MU.unsafeCopy (MU.unsafeSlice 0 size vs_') vs_
    MV.unsafeCopy (MV.unsafeSlice 0 size as') as
    push vs' as'
  else do
    push vs as

getLog_ :: Log a -> Version -> IO (Maybe a)
getLog_ (Log size vs as) v = do
  i <- searchVV vs 0 size v
  if i == size then
    return Nothing
  else
    Just <$> MV.unsafeRead as i

debugLog :: Log a -> IO [(Version, a)]
debugLog (Log size (VersionVector vs) as) = do
  for [0 .. size-1] $ \i -> do
    (,) <$> (Version <$> MU.read vs i) <*> MV.read as i

instance Logging Log where
  newLog = newLog_
  pushLog = pushLog_
  getLog = getLog_

