
module Math.Integration (
    simpson, adaptiveSimpson, adaptiveSimpsonLineIntegral,
    circle, circle',
    lineSegment, lineSegment',
    box, box',
    glueCurves
) where

import Data.Matrix as M

import Math.LinearAlgebra

simpson' :: Mat -> Mat -> Mat -> Double -> Double -> Mat
simpson' fa fb fm a b =
    factor *. (fa + 4 *. fm + fb)
  where
    factor = (b-a)/6

simpson :: (Double -> Mat) -> Double -> Double -> Mat
simpson f a b = simpson' (f a) (f b) (f m) a b
  where
    m = (b+a)/2

-- This private function is somewhat complicated to avoid redundant calls to
-- simpson' or to f
adaptiveSimpson' ε f a b m fa fb fm sab
  | normP (1/0) (sam + smb - sab) / 15 < ε = sam + smb
  | otherwise =
        adaptiveSimpson' ε f a m a_m fa fm fa_m sam
      + adaptiveSimpson' ε f m b m_b fm fb fm_b smb
  where
    a_m = (a+m)/2
    fa_m = f a_m
    sam = simpson' fa fm fa_m a m
    m_b = (m+b)/2
    fm_b = f m_b
    smb = simpson' fm fb fm_b m b

adaptiveSimpson :: Double -> (Double -> Mat) -> Double -> Double -> Mat
adaptiveSimpson ε f a b =
    adaptiveSimpson' ε f a b m fa fb fm $ simpson' fa fb fm a b
  where
    m = (a+b)/2
    fa = f a
    fb = f b
    fm = f m

adaptiveSimpsonLineIntegral
    :: Double
    -> (Double -> k)
    -> (Double -> Mat)
    -> (k -> Mat)
    -> Double
    -> Double
    -> Mat
adaptiveSimpsonLineIntegral ε γ γ' ω a b =
    adaptiveSimpson ε f a b
  where
    f t = (ω $ γ t) * γ' t

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
