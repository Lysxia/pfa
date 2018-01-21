module Data.PFA.Internal.Version where

import Data.Coerce
import qualified Data.Vector.Unboxed.Mutable as MU

newtype Version = Version Word deriving (Eq, Ord, Show)

zeroVersion :: Version
zeroVersion = Version 0

nextVersion :: Version -> Version
nextVersion (Version v) = Version (v + 1)

-- | Ideally we would just use @GeneralizedNewtypeDeriving@ for
-- @'MU.Unbox' 'Version'@.
--
-- Not threadsafe!
newtype VersionVector = VersionVector PreVV

type PreVV = MU.IOVector Word

lengthVV :: VersionVector -> Int
lengthVV (VersionVector vs_) = MU.length vs_

-- | Allocate a 'VersionVector'.
newVV :: Int -> IO VersionVector
newVV = coerce (MU.new :: Int -> IO PreVV)

unsafeWriteVV :: VersionVector -> Int -> Version -> IO ()
unsafeWriteVV = coerce (MU.unsafeWrite :: PreVV -> Int -> Word -> IO ())

unsafeReadVV :: VersionVector -> Int -> IO Version
unsafeReadVV = coerce (MU.unsafeRead :: PreVV -> Int -> IO Word)

-- | Given @vs :: 'VersionVector'@ and @v :: 'Version'@,
-- return the greatest index @i@ in the interval @[i0 .. j0-1]@ such that
-- @v <= vs[i]@.
searchVV :: VersionVector -> Int -> Int -> Version -> IO Int
searchVV vs i0 j0 v = search i0 j0
  where
    search :: Int -> Int -> IO Int
    search i j
      | i == j = return i
      | otherwise = do
        let k = (i + j) `div` 2
        v' <- unsafeReadVV vs k
        if v' < v then
          search (k+1) j
        else
          search i k
