{- | Port of the implementation in the original paper by Kumar, Blelloch, Harper.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.PFA.Kumar where

import Data.Traversable

import Control.Monad
import Data.Atomics
import Data.Foldable
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU

type Size = Int

newtype Version = Version Word deriving (Eq, Ord, Show)
newtype VersionVector = VersionVector (MU.IOVector Word)

-- | Invariant: the two vectors have the same length (capacity)
-- and the size is less than or equal to the capacity.
data Log a = Log !Size !VersionVector !(MV.IOVector a)

newLog_ :: Int -> IO (Log a)
newLog_ n =
  Log 0 <$> (VersionVector <$> MU.new n) <*> MV.new n

pushLog_ :: Log a -> Version -> a -> IO (Log a)
pushLog_ log@(Log size (VersionVector vs_) as) (Version v_) a = do
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

class Logging log where
  newLog :: Int -> IO (log a)
  pushLog :: log a -> Version -> a -> IO (log a)
  getLog :: log a -> Version -> IO (Maybe a)

instance Logging Log where
  newLog = newLog_
  pushLog = pushLog_
  getLog = getLog_

data PFA log a = PFA !(Ticket Version) !(IORef Version) !(MV.IOVector a) !(MV.IOVector (log a))

newIO :: Logging log => Int -> a -> IO (PFA log a)
newIO n a = do
  vRef <- newIORef (Version 0)
  v <- readForCAS vRef
  PFA v vRef <$> MV.replicate n a <*> MV.replicateM n (newLog 1)
{-# SPECIALIZE newIO :: Int -> a -> IO (PFA Log a) #-}

getIO :: Logging log => PFA log a -> Int -> IO a
getIO (PFA v vRef as ls) i = do
  guess <- MV.read as i  -- Read before comparing versions
  v' <- readIORef vRef
  let v_ = peekTicket v
  if v_ == v' then
    return guess
  else do
    l <- MV.unsafeRead ls i
    fromMaybe guess <$> getLog l v_
{-# SPECIALIZE getIO :: PFA Log a -> Int -> IO a #-}

setIO :: Logging log => PFA log a -> Int -> a -> IO (PFA log a)
setIO (PFA v vRef as ls) i a = do
  let n = MV.length as
      v_@(Version v_') = peekTicket v
  if fromIntegral v_' == n then do
    vRef' <- newIORef (Version 0)
    v' <- readForCAS vRef'
    as' <- MV.clone as
    ls' <- MV.replicateM n (newLog 1)
    MV.write as' i a
    return (PFA v' vRef' as' ls')
  else do
    (leaf, v') <- casIORef vRef v (Version (v_' + 1))
    if leaf then do
      a' <- MV.read as i
      l <- MV.unsafeRead ls i
      l' <- pushLog l v_ a'
      MV.unsafeWrite ls i l'
      MV.unsafeWrite as i a  -- In this order!
      return (PFA v' vRef as ls)
    else do
      vRef' <- newIORef (Version 0)
      v0' <- readForCAS vRef'
      let generate n f = do
            as' <- MV.unsafeNew n
            for_ [0 .. n-1] $ \i -> do
              a <- f i
              MV.unsafeWrite as' i a
            return as'
      as' <- generate n (\i -> do
        l <- MV.unsafeRead ls i
        a' <- getLog l v_
        case a' of
          Nothing -> MV.unsafeRead as i
          Just a -> return a)
      ls' <- MV.replicateM n (newLog 1)
      MV.write as' i a
      return (PFA v0' vRef' as' ls')
{-# SPECIALIZE setIO :: PFA Log a -> Int -> a -> IO (PFA Log a) #-}

debugIO :: Show a => PFA Log a -> IO ()
debugIO (PFA v vRef as ls) = do
  v' <- readIORef vRef
  as_ <- traverse (\i -> MV.read as i) [0 .. MV.length as - 1]
  ls_ <- traverse (\i -> MV.read ls i >>= debugLog) [0 .. MV.length ls - 1]
  print $ "V: " ++ show (v, v', as_, ls_)
