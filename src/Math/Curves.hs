
module Math.Curves (
    Curve(..),
    at, d
) where

import Math.LinearAlgebra

data Curve
  = Box Mat Double Double
  | Circle Mat Double
  | Segment Mat Mat
  | Constant Mat
  | General (Double -> Mat) (Double -> Mat)

at :: Curve -> Double -> Mat
at (Box lowerLeft width height) =
    glueCurves
        [ lineSegment c1 c2
        , lineSegment c2 c3
        , lineSegment c3 c4
        , lineSegment c4 c1
        ]
  where
    e1 = col [width, 0]
    e2 = col [0, height]
    c1 = lowerLeft
    c2 = lowerLeft + e1
    c3 = c2 + e2
    c4 = lowerLeft + e2
at (Circle center radius) = \t ->
    col [radius*cos (2*pi*t), radius*sin (2*pi*t)] + center
at (Segment p q) = \t -> (1-t) *. p + t *. q
at (Constant c) = \_ -> c
at (General f _) = f

d :: Curve -> Double -> Mat
d (Box _ width height) =
    glueCurves [\_ -> e1, \_ -> e2, \_ -> negate e1, \_ -> negate e2]
  where
    e1 = col [4*width, 0] :: Mat
    e2 = col [0, 4*height]
d (Circle _ radius) = \t ->
    col [-factor*sin (2*pi*t), factor*cos (2*pi*t)]
  where
    factor = 2*pi*radius
d (Segment p q) = \_ -> q - p
d (Constant _) = \_ -> col [0, 0]
d (General _ f') = f'

lineSegment :: Mat -> Mat -> Double -> Mat
lineSegment p q t = (1-t) *. p + t *. q

glueCurves :: [Double -> a] -> Double -> a
glueCurves curves t
  | flr >= fromIntegral len = last curves 1
  | otherwise = (curves !! flr) t''
  where
    len = length curves
    t' = t * fromIntegral len
    flr = floor t'
    t'' = t' - (fromIntegral flr) :: Double

