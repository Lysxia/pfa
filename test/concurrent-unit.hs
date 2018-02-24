{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad (when, replicateM_, replicateM)
import System.Console.AsciiProgress
  (newProgressBar, def, ProgressBar, pgTotal, tick, displayConsoleRegions)
import System.Console.Concurrent (outputConcurrent)
import System.Exit

import Data.PFA.Kumar
import Data.PFA.Internal.Log.Class
import qualified Data.PFA.Internal.Log.Chunks as LogC

main :: IO ()
main = displayConsoleRegions $ do
  c <- atomically newTChan
  pg <- newProgressBar (def{pgTotal = toInteger (retries * setIter)})
  replicateM_ retries (batch c pg)

batch :: TChan (Int, Int) -> ProgressBar -> IO ()
batch c pg = do
  forkIO (do
    v <- newIO 1 setIter
    go c setIter (v :: PFA LogC.Log Int))
  replicateM_ setIter (do
    (i, y) <- atomically (readTChan c)
    tick pg
    when (y /= i * getIter) (do
      outputConcurrent ("Counterexample: " ++ show (i, y) ++ "\n")
      exitFailure))

-- Number of batches
retries :: Int
retries = 10

-- Number of 'setIO' per batch
setIter :: Int
setIter = 1000000

-- Number of 'getIO' per 'setIO'
getIter :: Int
getIter = 50

-- Invariant: @v@ should be a vector of length 1 with value @i@.
go :: Logging log => TChan (Int, Int) -> Int -> PFA log Int -> IO ()
go _ 0 _ = return ()
go c i v = do
  forkIO (do
    ys <- replicateM getIter (getIO v 0)  -- 'getIO v 0' should always return the same result
    let !y = sum ys
    atomically (writeTChan c (i, y)))
  v <- setIO v 0 (i-1)
  go c (i-1) v
