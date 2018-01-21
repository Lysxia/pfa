{-# LANGUAGE BangPatterns #-}

import Control.Monad.Random.Class
import Data.Function (fix)
import qualified Data.IntMap.Strict as M
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Criterion.Main

import Data.PFA.Kumar
import Data.PFA.Internal.Log.Class
import qualified Data.PFA.Internal.Log.Vector as LogV
import qualified Data.PFA.Internal.Log.Chunks as LogC

-- A wrapper for vector contents we don't really care about.
newtype A = A Int

-- PFA implementations
data PFADict v a = PFADict
  { new :: Int -> a -> IO v
  , get :: v -> Int -> IO a
  , set :: v -> Int -> a -> IO v
  }

-- A dictionary that does nothing at all, provides a lower bound on the optimal
-- performance of a PFA implementation. Measures the cost of the surrounding
-- benchmarking code.
baselinePFA :: PFADict () A
baselinePFA = PFADict
  (\_ _ -> return ())
  (\_ _ -> return (A 0))
  (\_ _ _ -> return ())
{-# INLINE baselinePFA #-}

-- The original implementation
originalPFA :: Logging log => PFADict (PFA log a) a
originalPFA = PFADict newIO getIO setIO
{-# SPECIALIZE originalPFA :: PFADict (PFA LogV.Log a) a #-}
{-# SPECIALIZE originalPFA :: PFADict (PFA LogC.Log a) a #-}
{-# INLINE originalPFA #-}

-- A mutable vector implementation with destructive updates:
-- must not be used nonlinearly.
vectorPFA :: PFADict (MV.IOVector Int) A
vectorPFA = PFADict
  { new = \n (A a) -> MV.replicate n a
  , get = \v i -> A <$> MV.read v i
  , set = \v i (A a) -> MV.write v i a >> return v
  }

unboxedPFA :: PFADict (MU.IOVector Int) A
unboxedPFA = PFADict
  { new = \n (A a) -> MU.replicate n a
  , get = \v i -> A <$> MU.read v i
  , set = \v i (A a) -> MU.write v i a >> return v
  }
{-# INLINE vectorPFA #-}

-- A Map-based implementation (from containers)
mapPFA :: PFADict (M.IntMap a) a
mapPFA = PFADict
  { new = \n a -> return $! M.fromDistinctAscList [(i, a) | i <- [0 .. n-1]]
  , get = \v i -> return $! v M.! i
  , set = \v i a -> return $! M.insert i a v
  }
{-# INLINE mapPFA #-}

-- Benchmarking different combinations of operations
data Mode = GetOnly | SetOnly | GetAndSet
  deriving Show

allModes :: [Mode]
allModes = [GetOnly, SetOnly, GetAndSet]
{-# INLINE allModes #-}

benchPFA
  :: PFADict v A
  -> Mode
  -> Int           -- ^ Length of vectors
  -> U.Vector Int  -- ^ Random values less than length
  -> IO A
benchPFA pfa mode n xs = do
  v0 <- new pfa n (A 0)  -- just a dummy value
  let setThen k !a !i v =
        if i < U.length xs then
          set pfa v (xs U.! i) (A i) >>= k a (i+1)
        else return a
      getThen k !(A a) !i v =
        if i < U.length xs then
          get pfa v (xs U.! i) >>= \(A a') -> k (A (a + a')) (i+1) v
        else return (A a)
  case mode of
    GetOnly -> fix getThen (A 0) 0 v0
    SetOnly -> fix setThen (A 0) 0 v0
    GetAndSet -> fix (setThen . getThen) (A 0) 0 v0
{-# INLINE benchPFA #-}

-- Weird @flip map@ with explicit cases for inlining.
(<&>) :: [a] -> (a -> b) -> [b]
(<&>) [] _f = []
(<&>) [a] f = [f a]
(<&>) [a, b] f = [f a, f b]
(<&>) [a, b, c] f = [f a, f b, f c]
(<&>) [a, b, c, d] f = [f a, f b, f c, f d]
(<&>) xs f = map f xs
{-# INLINE (<&>) #-}

main :: IO ()
main = defaultMain $
  [10, 100] <&> \n -> bgroup ("length-" ++ show n) $
  allModes <&> run n

run :: Int -> Mode -> Benchmark
run n mode =
  env (U.replicateM 1000 (getRandomR (0, n-1))) $ \xs ->
    bgroup (show mode) $
      let groupWith name pfa =
            bench name . whnfIO $ benchPFA pfa mode n xs
          {-# INLINE groupWith #-}
      in
      [ groupWith "baseline" baselinePFA
      , groupWith "unboxed"  unboxedPFA
      , groupWith "vector"   vectorPFA
      , groupWith "original" (originalPFA :: PFADict (PFA LogV.Log A) A)
      , groupWith "chunks"   (originalPFA :: PFADict (PFA LogC.Log A) A)
      , groupWith "map"      mapPFA
      ]
{-# INLINE run #-}
