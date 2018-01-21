{- | Port of the implementation in the original paper by Kumar, Blelloch, Harper.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.PFA.Kumar where

import Data.Atomics
import Data.Foldable
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Mutable as MV

import Data.PFA.Internal.Log.Class
import qualified Data.PFA.Internal.Log.Chunks as LogC
import qualified Data.PFA.Internal.Log.Vector as LogV
import Data.PFA.Internal.Version

data PFA log a = PFA !(Ticket Version) !(IORef Version) !(MV.IOVector a) !(MV.IOVector (log a))

newIO :: Logging log => Int -> a -> IO (PFA log a)
newIO n a = do
  vRef <- newIORef (Version 0)
  v <- readForCAS vRef
  PFA v vRef <$> MV.replicate n a <*> MV.replicateM n (newLog 1)
{-# SPECIALIZE newIO :: Int -> a -> IO (PFA LogV.Log a) #-}
{-# SPECIALIZE newIO :: Int -> a -> IO (PFA LogC.Log a) #-}

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
{-# SPECIALIZE getIO :: PFA LogV.Log a -> Int -> IO a #-}
{-# SPECIALIZE getIO :: PFA LogC.Log a -> Int -> IO a #-}

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
{-# SPECIALIZE setIO :: PFA LogV.Log a -> Int -> a -> IO (PFA LogV.Log a) #-}
{-# SPECIALIZE setIO :: PFA LogC.Log a -> Int -> a -> IO (PFA LogC.Log a) #-}

debugIO :: Show a => PFA LogV.Log a -> IO ()
debugIO (PFA v vRef as ls) = do
  v' <- readIORef vRef
  as_ <- traverse (\i -> MV.read as i) [0 .. MV.length as - 1]
  ls_ <- traverse (\i -> MV.read ls i >>= LogV.debugLog) [0 .. MV.length ls - 1]
  print $ "V: " ++ show (v, v', as_, ls_)
