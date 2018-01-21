module Data.PFA.Internal.Log.Vector where

import Data.Traversable
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU

import Data.PFA.Internal.Log.Class

type Size = Int

newtype VersionVector = VersionVector (MU.IOVector Word)

-- | Invariant: the two vectors have the same length (capacity)
-- and the size is less than or equal to the capacity.
data Log a = Log !Size !VersionVector !(MV.IOVector a)

newLog_ :: Int -> IO (Log a)
newLog_ n =
  Log 0 <$> (VersionVector <$> MU.new n) <*> MV.new n

pushLog_ :: Log a -> Version -> a -> IO (Log a)
pushLog_ (Log size (VersionVector vs_) as) (Version v_) a = do
  let push vs_ as = do
        MU.unsafeWrite vs_ size v_ :: IO ()
        MV.unsafeWrite as size a
        return (Log (size + 1) (VersionVector vs_) as)
  if size == MU.length vs_ then do
    Log _ (VersionVector vs'_) as' <- newLog (size * 2)
    MU.unsafeCopy (MU.unsafeSlice 0 size vs'_) vs_
    MV.unsafeCopy (MV.unsafeSlice 0 size as') as
    push vs'_ as'
  else do
    push vs_ as

getLog_ :: Log a -> Version -> IO (Maybe a)
getLog_ (Log size (VersionVector vs) as) (Version v) = do
  let search :: Int -> Int -> IO Int
      search i j
        | i == j = return i
        | otherwise = do
        v' <- MU.unsafeRead vs k
        if v' < v then
          search (k+1) j
        else
          search i k
       where k = (i + j) `div` 2
  i <- search 0 size
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

