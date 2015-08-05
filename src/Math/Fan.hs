
module Math.Fan (
    Fan(..),
    indexI,
    safeIndexI,
    unsafeIndexI,
    indexO,
    safeIndexO,
    findOuterAt,
    findOuterAtLinear,
    findOuterAtBinary
) where

import Data.Bits (shiftR)

import Data.Vector as V

data Fan outer inner = Fan (V.Vector (outer, inner)) outer
    deriving (Eq)

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
    | otherwise = fst $ vec `unsafeIndex` i

cons :: (outer, inner) -> Fan outer inner -> Fan outer inner
cons oi (Fan vec last) = Fan (V.cons oi vec) last

snoc :: Fan outer inner -> (inner, outer) -> Fan outer inner
snoc (Fan vec last) (i, o) = Fan (V.snoc vec (last, i)) o

-- performance note: of course findOuterAtLinear is faster for small
-- Vectors (say up to about 20 elements when comparing Ints).
-- But checking for length and then choosing Linear or Binary is almost always
-- slower than just using Binary anyway
findOuterAt :: (inner -> Ordering) -> Fan outer inner -> outer
findOuterAt = findOuterAtBinary

-- performance note: I experimented with first checking if 
-- compare (V.last vec) == GT
-- That way we would not have to check bounds while iterating through vec.
-- But that is somehow *slower*.
findOuterAtLinear :: (inner -> Ordering) -> Fan outer inner -> outer
findOuterAtLinear compare (Fan vec last) =
    case V.find (\(_, i) -> not $ compare i == GT) vec of
        Just (val, _) -> val
        Nothing -> last

findOuterAtBinary :: (inner -> Ordering) -> Fan outer inner -> outer
findOuterAtBinary compare (Fan vec last) =
    loop 0 (V.length vec)
  where
    loop lower upper
      | lower >= upper =
          if lower >= V.length vec
             then last
             else fst $ vec V.! lower
      | otherwise = case compare (snd $ V.unsafeIndex vec k) of
            LT -> loop lower k
            GT -> loop (k+1) upper
            EQ -> fst $ V.unsafeIndex vec k
      where k = shiftR (upper + lower) 1

