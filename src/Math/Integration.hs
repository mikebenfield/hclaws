
module Math.Integration (
    simpson, adaptiveSimpson, adaptiveSimpsonLineIntegral
) where

import Math.Curves
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
    -> Curve
    -> (Point -> Mat)
    -> Double
    -> Double
    -> Mat
adaptiveSimpsonLineIntegral ε γ ω a b =
    adaptiveSimpson ε f a b
  where
    f s = (ω $ curve γ s) * tan s
    tan s = col [x $ tangent γ s, t $ tangent γ s]
