{-# LANGUAGE ScopedTypeVariables #-}

import Data.Proxy
import GHC.Stack

import Data.PFA.Kumar
import Data.PFA.Internal.Log.Class
import qualified Data.PFA.Internal.Log.Chunks as LogC
import qualified Data.PFA.Internal.Log.Vector as LogV

aeq :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
aeq a b
  | a == b = return ()
  | otherwise = error $ "assertion failed: " ++ show a ++ " /= " ++ show b

mainWith :: forall log. Logging log => Proxy log -> IO ()
mainWith _ = do
  v0 <- newIO 3 (20 :: Int)
  x <- getIO (v0 :: PFA log Int) 0
  aeq x 20
  v1 <- setIO v0 1 30
  v2 <- setIO v1 1 40
  v3 <- setIO v2 2 50
  v4 <- setIO v3 1 55
  x <- getIO v1 1
  aeq x 30
  x <- getIO v0 1
  aeq x 20
  x <- getIO v3 1
  aeq x 40
  v2' <- setIO v1 2 60
  x <- getIO v2' 1
  aeq x 30

main = do
  mainWith (Proxy :: Proxy LogV.Log)
  mainWith (Proxy :: Proxy LogC.Log)
