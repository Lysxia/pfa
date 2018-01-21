module Data.PFA.Internal.Log.Chunks where

import qualified Data.Vector.Mutable as MV

import Data.PFA.Internal.Log.Class
import Data.PFA.Internal.Version

type Size = Int

data Log a
  = Chunk !Size !VersionVector !(MV.IOVector a) !(Log a)
  | Nil

newLog_ :: IO (Log a)
newLog_ = return Nil

getLog_ :: Log a -> Version -> IO (Maybe a)
getLog_ c v = getLog_' c v (return Nothing)

getLog_' :: Log a -> Version -> IO (Maybe a) -> IO (Maybe a)
getLog_' c v def = case c of
  Nil -> def
  Chunk size vs as c -> do
    v' <- unsafeReadVV vs 0
    if v' < v then do
      i <- searchVV vs 1 size v
      if i == size then
        def
      else
        Just <$> MV.unsafeRead as i
    else
      getLog_'' c v as
{-# INLINE getLog_' #-}

getLog_'' :: Log a -> Version -> MV.IOVector a -> IO (Maybe a)
getLog_'' c v as = getLog_' c v (Just <$> MV.unsafeRead as 0)

initialCapacity :: Int
initialCapacity = 4

pushLog_ :: Log a -> Version -> a -> IO (Log a)
pushLog_ Nil v a = newChunk initialCapacity v a Nil
pushLog_ c@(Chunk size vs _  _) v a | size == lengthVV vs = newChunk (size * 2) v a c
pushLog_ (Chunk size vs as c) v a = do
  unsafeWriteVV vs size v
  MV.unsafeWrite as size a
  return $ Chunk (size + 1) vs as c

newChunk :: Int -> Version -> a -> Log a -> IO (Log a)
newChunk n v a c = do
  vs <- newVV n
  as <- MV.new n
  unsafeWriteVV vs 0 v
  MV.unsafeWrite as 0 a
  return (Chunk 1 vs as c)

instance Logging Log where
  newLog = newLog_
  getLog = getLog_
  pushLog = pushLog_
