
module Math.Fan (
    Fan(..),
    iLength,
    indexI,
    safeIndexI,
    unsafeIndexI,
    indexO,
    safeIndexO,
    unsafeIndexO,
    lastO,
    firstO,
    cons,
    snoc,
    findOuterAt,
    findOuterAtLinear,
    findOuterAtBinary,
    findIndexAt,
    findIndexAtLinear,
    findIndexAtBinary
) where

import Data.Bits (shiftR)

import qualified Data.Vector as V

data Fan outer inner = Fan (V.Vector (outer, inner)) outer
    deriving (Eq)

iLength :: Fan outer inner -> Int
iLength (Fan vec _) = V.length vec

indexI :: Fan outer inner -> Int -> inner
indexI (Fan vec _) i = snd $ vec V.! i

safeIndexI :: Fan outer inner -> Int -> Maybe inner
safeIndexI (Fan vec _) i = vec V.!? i >>= return . snd

unsafeIndexI :: Fan outer inner -> Int -> inner
unsafeIndexI (Fan vec _) i = snd $ vec `V.unsafeIndex` i

indexO :: Fan outer inner -> Int -> outer
indexO (Fan vec last) i
    | i == V.length vec = last
    | otherwise = fst $ vec V.! i

safeIndexO :: Fan outer inner -> Int -> Maybe outer
safeIndexO (Fan vec last) i
    | i == V.length vec = Just last
    | otherwise = vec V.!? i >>= return . fst

unsafeIndexO :: Fan outer inner -> Int -> outer
unsafeIndexO (Fan vec last) i
    | i == V.length vec = last
    | otherwise = fst $ vec `V.unsafeIndex` i

lastO :: Fan outer inner -> outer
lastO (Fan _ last) = last

firstO :: Fan outer inner -> outer
firstO (Fan vec _) | V.length vec > 0 = fst $ vec V.! 0
firstO (Fan _ last) = last

cons :: (outer, inner) -> Fan outer inner -> Fan outer inner
cons oi (Fan vec last) = Fan (V.cons oi vec) last

snoc :: Fan outer inner -> (inner, outer) -> Fan outer inner
snoc (Fan vec last) (i, o) = Fan (V.snoc vec (last, i)) o

findOuterAt :: (inner -> Ordering) -> Fan outer inner -> outer
findOuterAt compare fan = indexO fan $ findIndexAt compare fan

findOuterAtLinear :: (inner -> Ordering) -> Fan outer inner -> outer
findOuterAtLinear compare fan = indexO fan $ findIndexAtLinear compare fan

findOuterAtBinary :: (inner -> Ordering) -> Fan outer inner -> outer
findOuterAtBinary compare fan = indexO fan $ findIndexAtBinary compare fan

-- performance note: of course findOuterAtLinear is faster for small
-- Vectors (say up to about 20 elements when comparing Ints).
-- But checking for length and then choosing Linear or Binary is almost always
-- slower than just using Binary anyway
findIndexAt :: (inner -> Ordering) -> Fan outer inner -> Int
findIndexAt = findIndexAtBinary

-- performance note: I experimented with first checking if
-- compare (V.last vec) == GT
-- That way we would not have to check bounds while iterating through vec.
-- But that is somehow *slower*.
findIndexAtLinear :: (inner -> Ordering) -> Fan outer inner -> Int
findIndexAtLinear compare (Fan vec _) =
    case V.findIndex (\(_, i) -> not $ compare i == GT) vec of
        Just j -> j
        Nothing -> V.length vec

findIndexAtBinary :: (inner -> Ordering) -> Fan outer inner -> Int
findIndexAtBinary compare (Fan vec _) =
    loop 0 (V.length vec)
  where
    loop lower upper
      | lower >= upper =
          if lower >= V.length vec
             then V.length vec
             else lower
      | otherwise = case compare (snd $ V.unsafeIndex vec k) of
            LT -> loop lower k
            GT -> loop (k+1) upper
            EQ -> k
      where k = shiftR (upper + lower) 1
