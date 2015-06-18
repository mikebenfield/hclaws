
module Math.Integration (
  simpson, adaptiveSimpson , adaptiveSimpsonLineIntegral,

  circle, circle',
  lineSegment, lineSegment',
  box, box',
  glueCurves
  ) where

import Data.Matrix as M

import Math.LinearAlgebra

simpson :: Vector v => (Double -> v) -> Double -> Double -> v
simpson f a b = factor *. (f a +. 4*.f m +. f b)
  where
    m = (b+a)/2
    factor = (b-a)/6

adaptiveSimpson :: Normed v => Double -> (Double -> v) -> Double -> Double -> v
adaptiveSimpson ε f a b = 
  if norm ((sac +. scb) -. sab) / 15 < ε then
    sac +. scb
  else
    adaptiveSimpson ε f a c +. adaptiveSimpson ε f c b
  where
    c = (b+a)/2
    sab = simpson f a b
    sac = simpson f a c
    scb = simpson f c b

--adaptiveSimpsonMat :: Double -> (Double -> Mat) -> Double -> Double -> Mat
--adaptiveSimpsonMat ε f a b = fmap 

adaptiveSimpsonLineIntegral :: Double->
                               (Double -> Mat) ->
                               (Double -> Mat) ->
                               (Mat -> Mat) ->
                               Double ->
                               Double ->
                               Double
adaptiveSimpsonLineIntegral ε γ γ' ω a b =
  adaptiveSimpson ε f a b
  where
    f t = ((ω $ γ t) * γ' t) M.! (1, 1)

-- some useful curves in R^2

circle :: Mat -> Double -> Double -> Mat
circle center radius t =
  col [radius*cos (2*pi*t), radius*sin (2*pi*t)] + center

circle' :: Double -> Double -> Mat
circle' radius t =
  col [-factor*sin (2*pi*t), factor*cos (2*pi*t)] 
  where
    factor = 2*pi*radius

lineSegment :: Mat -> Mat -> Double -> Mat
lineSegment p q t = scaleMatrix (1-t) p + scaleMatrix t q

lineSegment' :: Mat -> Mat -> Double -> Mat
lineSegment' p q _ = q - p

glueCurves :: [Double -> a] -> Double -> a
glueCurves curves t =
  curve t''
  where
    len = length curves
    t' = t * fromIntegral len
    flr = floor t'
    t'' = t' - (fromIntegral flr) :: Double
    curve | flr >= fromIntegral len = last curves
          | otherwise = curves !! flr

box :: Mat -> Double -> Double -> Double -> Mat
box lowerLeft width height =
  glueCurves [lineSegment c1 c2,
              lineSegment c2 c3,
              lineSegment c3 c4,
              lineSegment c4 c1]
  where
    e1 = col [width, 0]
    e2 = col [0, height]
    c1 = lowerLeft
    c2 = lowerLeft + e1
    c3 = c2 - e2
    c4 = lowerLeft + e1

box' :: Double -> Double -> Double -> Mat
box' width height =
  glueCurves [\_ -> e1, \_ -> e2, \_ -> negate e1, \_ -> negate e2]
  where
    e1 = col [width, 0] :: Mat
    e2 = col [0, height]

---- γ is the curve we are integrating over
---- ω is a 1-form: interpret its components as the coefficients of dx^i
--trapezoidLineIntegral :: (Double -> Vector Double) -> (Vector Double -> Vector Double)
--                       -> Double -> Double -> Double
--trapezoidLineIntegral γ ω a b =
--  sum $ map ith [0..len-1] 
--  where
--    m = (b+a)/2
--    γa = γ a
--    γb = γ b
--    ωa = ω $ γa
--    ωb = ω $ γb
--    ωm = ω $ γm
--    len = length ωa
--    --factor i = (γb ! i - γa ! i) / 6
--    --ith i = factor i * (ωa ! i + 4 * ωm ! i + ωb ! i)
--    ith i = (γb ! i - γa ! i) * (ωa ! i + ωb ! i) / 2
--
--adaptiveTrapezoidLineIntegral :: Double ->
--                                 (Double -> Vector Double) ->
--                                 (Vector Double -> Vector Double) ->
--                                 Double -> Double -> Double
--adaptiveTrapezoidLineIntegral ε γ ω a b =
--  if abs ((tac + tcb) - tab) / 15 < ε then
--    tac + tcb
--  else
--    adaptiveTrapezoidLineIntegral ε γ ω a c +
--    adaptiveTrapezoidLineIntegral ε γ ω c b
--
