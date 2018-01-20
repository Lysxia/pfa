{- | Port of the implementation in the original paper by Kumar, Blelloch, Harper.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.PFA.Kumar where

import Data.Traversable

import Control.Concurrent.STM
import Control.Monad
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

newLog :: Int -> IO (Log a)
newLog n =
  Log 0 <$> (VersionVector <$> MU.new n) <*> MV.new n

pushLog :: Log a -> Version -> a -> IO (Log a)
pushLog log@(Log size (VersionVector vs_) as) (Version v_) a = do
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

getLog :: Log a -> Version -> IO (Maybe a)
getLog (Log size (VersionVector vs) as) (Version v) = do
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

data PFA a = PFA !Version !(TVar Version) !(MV.IOVector a) !(MV.IOVector (Log a))

newIO :: Int -> a -> IO (PFA a)
newIO n a = PFA v0 <$> newTVarIO v0 <*> MV.replicate n a <*> MV.replicateM n (newLog 1)
  where
    v0 = Version 0

getIO :: PFA a -> Int -> IO a
getIO (PFA v vRef as ls) i = do
  guess <- MV.read as i  -- Read before comparing versions
  leaf <- do
    v' <- readTVarIO vRef
    return (v == v')
  if leaf then
    return guess
  else do
    l <- MV.unsafeRead ls i
    fromMaybe guess <$> getLog l v

setIO :: PFA a -> Int -> a -> IO (PFA a)
setIO (PFA v@(Version v_) vRef as ls) i a = do
  let n = MV.length as
      v0 = Version 0
  if fromIntegral v_ == n then do
    vRef' <- newTVarIO v0
    as' <- MV.clone as
    ls' <- MV.replicateM n (newLog 1)
    MV.write as' i a
    return (PFA v0 vRef' as' ls')
  else do
    leaf <- atomically $ do  -- compare and swap
      v' <- readTVar vRef
      if v == v' then do
        writeTVar vRef $! Version (v_ + 1)
        return True
      else
        return False
    if leaf then do
      a' <- MV.read as i
      l <- MV.unsafeRead ls i
      l' <- pushLog l v a'
      MV.unsafeWrite ls i l'
      MV.unsafeWrite as i a  -- In this order!
      return (PFA (Version (v_ + 1)) vRef as ls)
    else do
      vRef' <- newTVarIO v0
      let generate n f = do
            as' <- MV.unsafeNew n
            for_ [0 .. n-1] $ \i -> do
              a <- f i
              MV.unsafeWrite as' i a
            return as'
      as' <- generate n (\i -> do
        l <- MV.unsafeRead ls i
        a' <- getLog l v
        case a' of
          Nothing -> MV.unsafeRead as i
          Just a -> return a)
      ls' <- MV.replicateM n (newLog 1)
      MV.write as' i a
      return (PFA v0 vRef' as' ls')

debugIO :: Show a => PFA a -> IO ()
debugIO (PFA v vRef as ls) = do
  v' <- readTVarIO vRef
  as_ <- traverse (\i -> MV.read as i) [0 .. MV.length as - 1]
  ls_ <- traverse (\i -> MV.read ls i >>= debugLog) [0 .. MV.length ls - 1]
  print $ "V: " ++ show (v, v', as_, ls_)
