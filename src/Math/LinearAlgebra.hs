
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Math.LinearAlgebra (
    row, col, getRows, getCols,
    toList, m, s,
    (*.),
    Mat,
    normP,
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

m :: (a -> b) -> (M.Matrix a -> M.Matrix b)
m f x = col [f $ x M.! (1, 1)]

s :: (M.Matrix a -> M.Matrix b) -> (a -> b)
s f x = (f $ col [x]) M.! (1, 1)

infixl 7 *.
(*.) :: Num a => a -> M.Matrix a -> M.Matrix a
(*.) = M.scaleMatrix

type Mat = M.Matrix Double

normP :: Double -> Mat -> Double
normP p m
  | p < 1 = error "normP called with p<1"
  | p == (1/0) = maximum $ map abs $ toList m
  | otherwise = (sum $ map (\x -> (abs x)**p) $ toList m)**(1/p)

