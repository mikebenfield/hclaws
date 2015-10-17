{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Hclaws.LinearAlgebra (
    Point(..),
    point,
    Normable(..),
) where

import qualified Math.FTensor.Algebra as A
import qualified Math.FTensor.General as F

data Point = Point
    { x :: !Double
    , t :: !Double
    }
    deriving (Show, Eq)

instance A.Additive Point where
    (+.) p1 p2 = Point (x p1 + x p2) (t p1 + t p2)

instance A.WithZero Point where
    zero = Point 0 0

instance A.WithNegatives Point where
    neg Point{..} = Point (negate x) (negate t)
    (-.) p1 p2 = Point (x p1 - x p2) (t p1 - t p2)

point :: Double -> Double -> Point
point x' t' = Point {x=x', t=t'}

class Normable a where
    normP :: Double -> a -> Double

instance Normable Double where
    normP _ = abs

instance Normable (F.TensorBoxed dims Double) where
    normP p t
      | p < 1 = error "normP called with p<1"
      | p == (1/0) = maximum $ fmap abs t
      | otherwise = (sum $ fmap (\x -> (abs x)**p) t)**(1/p)

instance Normable Point where
    normP p Point{..}
      | p < 1 = error "normP called with p<1"
      | p == (1/0) = max (abs x) (abs t)
      | otherwise = ((abs x)**p + (abs t)**p)**(1/p)
