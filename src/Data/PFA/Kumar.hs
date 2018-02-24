{- | Port of the implementation in the original paper by Kumar, Blelloch, Harper.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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

data PFA log a
  = PFA
      (Ticket Version)
      -- ^ This node's version
      -- (TODO: why does keeping this lazy make microbenchmarks faster?)
      !(IORef Version)        -- ^ Leaf version
      !(MV.IOVector a)        -- ^ Leaf vector
      !(MV.IOVector (log a))  -- ^ Logs

-- | @newIO n a@: Return an immutable array of length @n@ initialized with @a@.
newIO :: Logging log => Int -> a -> IO (PFA log a)
newIO n a = do
  vRef <- newIORef (Version 0)
  v <- readForCAS vRef
  PFA v vRef <$> MV.replicate n a <*> MV.replicateM n newLog
{-# SPECIALIZE newIO :: Int -> a -> IO (PFA LogV.Log a) #-}
{-# SPECIALIZE newIO :: Int -> a -> IO (PFA LogC.Log a) #-}

-- | @getIO v i@: Get the value at index @i@.
getIO :: Logging log => PFA log a -> Int -> IO a
getIO (PFA v vRef as ls) i = do
#ifndef MUTANT_GETIO
  guess <- MV.read as i  -- Read before comparing versions
  v' <- readIORef vRef   --
#else
  v' <- readIORef vRef   --
  guess <- MV.read as i  -- "Oops"
#endif
  let v_ = peekTicket v
  if v_ == v' then
    return guess
  else do
    l <- MV.unsafeRead ls i
    fromMaybe guess <$> getLog l v_
{-# INLINE getIO #-}
{-# SPECIALIZE getIO :: PFA LogV.Log a -> Int -> IO a #-}
{-# SPECIALIZE getIO :: PFA LogC.Log a -> Int -> IO a #-}

renewFactor :: Int
renewFactor = 4

-- | @setIO v i a@: Create an updated copy of @v@ where the @i@-th element is @a@.
--
-- @v@ still denotes the same sequence as before the update.
--
-- > do getIO v i
-- >    setIO v i a1  -- ignore new array
-- >    getIO v i
-- > =
-- > getIO v i
setIO :: Logging log => PFA log a -> Int -> a -> IO (PFA log a)
setIO (PFA v vRef as ls) !i a = do
  let n = MV.length as
      v_@(Version v_') = peekTicket v
  if fromIntegral v_' == renewFactor * n then do
    vRef' <- newIORef (Version 0)
    v' <- readForCAS vRef'
    as' <- MV.clone as
    ls' <- MV.replicateM n newLog
    MV.write as' i a
    return (PFA v' vRef' as' ls')
  else do
    (leaf, v') <- casIORef vRef v (Version (v_' + 1))
    if leaf then do
      a' <- MV.read as i
      l <- MV.unsafeRead ls i
      l' <- pushLog l v_ a'
#ifndef MUTANT_SETIO
      MV.unsafeWrite ls i l' --
      MV.unsafeWrite as i a  -- In this order!
#else
      MV.unsafeWrite as i a  -- "Oops"
      MV.unsafeWrite ls i l' --
#endif
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
      ls' <- MV.replicateM n newLog
      MV.write as' i a
      return (PFA v0' vRef' as' ls')
{-# INLINE setIO #-}
{-# SPECIALIZE setIO :: PFA LogV.Log a -> Int -> a -> IO (PFA LogV.Log a) #-}
{-# SPECIALIZE setIO :: PFA LogC.Log a -> Int -> a -> IO (PFA LogC.Log a) #-}

debugIO :: Show a => PFA LogV.Log a -> IO ()
debugIO (PFA v vRef as ls) = do
  v' <- readIORef vRef
  as_ <- traverse (\i -> MV.read as i) [0 .. MV.length as - 1]
  ls_ <- traverse (\i -> MV.read ls i >>= LogV.debugLog) [0 .. MV.length ls - 1]
  print $ "V: " ++ show (v, v', as_, ls_)
