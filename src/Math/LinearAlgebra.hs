
module Math.LinearAlgebra (
    Point(..),
    point,
    row,
    col,
    getRows,
    getCols,
    toList,
    (*.),
    Mat,
    normP,
) where

import qualified Data.Matrix as M
import qualified Data.Vector as V

data Point = Point
    { x :: !Double
    , t :: !Double
    }
    deriving (Show, Eq)

point :: Double -> Double -> Point
point x' t' = Point {x=x', t=t'}

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

infixl 7 *.
(*.) :: Num a => a -> M.Matrix a -> M.Matrix a
(*.) = M.scaleMatrix

type Mat = M.Matrix Double

normP :: Double -> Mat -> Double
normP p m
  | p < 1 = error "normP called with p<1"
  | p == (1/0) = maximum $ map abs $ toList m
  | otherwise = (sum $ map (\x -> (abs x)**p) $ toList m)**(1/p)
