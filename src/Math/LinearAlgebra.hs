
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Math.LinearAlgebra (
  row, col, getRows, getCols,

  toList, normP,

  Additive(..), Vector(..), Normed(..),

  Mat
  ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V

row :: [a] -> M.Matrix a
row list = M.fromList 1 (length list) list

col :: [a] -> M.Matrix a
col list = M.fromList (length list) 1 list

getRows :: M.Matrix a -> [V.Vector a]
getRows m = fmap (\i -> M.getRow i m) [1..M.nrows m]

getCols :: M.Matrix a -> [V.Vector a]
getCols m = fmap (\i -> M.getCol i m) [1..M.ncols m]

toList :: M.Matrix a -> [a]
toList m = [m M.! (i,j) | i <- [1, M.nrows m], j <- [1, M.ncols m]]

infixl 6 +., -.
class Additive a where
  (+.) :: a -> a -> a
  (-.) :: a -> a -> a

instance Num n => Additive n where
  (+.) = (+)
  (-.) = (-)

infixl 7  *. --, /.
-- Ultimately VectorSpace should work with different scalars than Doubles,
-- but I have not been able to get such a class to type check
class Additive v => Vector v where
  (*.) :: Double -> v -> v
  --(/.) :: v -> Scalar v -> v
  --v /. n = (1/n) *. v

instance Vector Double where
  (*.) = (*)

instance Vector (M.Matrix Double) where
  (*.) = M.scaleMatrix

type Mat = M.Matrix Double

class Vector v => Normed v where
  norm :: v -> Double

instance Normed Double where
  norm = abs

instance Normed Mat where
  norm = normP (1/0)

normP :: Double -> M.Matrix Double -> Double
normP p m
  | p < 1 = error "normP called with p<1"
  | p == (1/0) = maximum $ map abs $ toList m
  | otherwise = (sum $ map (\x -> (abs x)**p) $ toList m)**(1/p)

