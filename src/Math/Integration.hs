{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.Integration (
    simpson,
    adaptiveSimpson,
    adaptiveSimpsonLineIntegral,
    euler,
    rungeKutta
) where

import Data.Proxy
import GHC.TypeLits

import Math.FTensor.Algebra
import Math.FTensor.General as F

import Math.Curves
import Math.LinearAlgebra

simpson'
    :: (VectorSpace a, Scalar a ~ Double)
    => a
    -> a
    -> a
    -> Double
    -> Double
    -> a
simpson' fa fb fm a b = ((1/6)*(b-a)) *: (fa +. 4 *: fm +. fb)

simpson
    :: (VectorSpace a, Scalar a ~ Double)
    => (Double -> a)
    -> Double
    -> Double
    -> a
simpson f a b = simpson' (f a) (f b) (f m) a b
  where
    m = (b+a)*(1/2)

-- This private function is somewhat complicated to avoid redundant calls to
-- simpson' or to f
adaptiveSimpson'
    :: (VectorSpace a, Normable a, Scalar a ~ Double)
    => Double
    -> (Double -> a)
    -> Double
    -> Double
    -> Double
    -> a
    -> a
    -> a
    -> a
    -> a
adaptiveSimpson' ε f a b m fa fb fm sab
  | normP (1/0) (sam +. smb -. sab) / 15 < ε = sam +. smb
  | otherwise =
        adaptiveSimpson' ε f a m a_m fa fm fa_m sam +.
            adaptiveSimpson' ε f m b m_b fm fb fm_b smb
  where
    a_m = (a+m)*(1/2)
    fa_m = f a_m
    sam = simpson' fa fm fa_m a m
    m_b = (m+b)*(1/2)
    fm_b = f m_b
    smb = simpson' fm fb fm_b m b

adaptiveSimpson
    :: (VectorSpace a, Normable a, Scalar a ~ Double)
    => Double
    -> (Double -> a)
    -> Double
    -> Double
    -> a
adaptiveSimpson ε f a b =
    adaptiveSimpson' ε f a b m fa fb fm $ simpson' fa fb fm a b
  where
    m = (a+b)*(1/2)
    fa = f a
    fb = f b
    fm = f m

adaptiveSimpsonLineIntegral
    :: KnownNat n 
    => Double
    -> Curve
    -> (Point -> F.TensorBoxed '[n, 2] Double)
    -> Double
    -> Double
    -> F.TensorBoxed '[n] Double
adaptiveSimpsonLineIntegral ε γ ω a b =
    adaptiveSimpson ε f a b
  where
    f s = F.mul (ω $ curve γ s) (Proxy::Proxy 1) (tan s) (Proxy::Proxy 0)
    tan :: Double -> F.TensorBoxed '[2] Double
    tan s =
        let v :: Point
            v = tangent γ s
        in
        F.Tensor [x v, t v]

euler
    :: (VectorSpace a, Scalar a ~ Double)
    => Int
    -> (a -> Double -> a)
    -> a
    -> Double
    -> a
euler steps eqn x0 t
  | steps <= 0 = x0
  | otherwise = loop steps x0 0
  where
    stepSize = t / fromIntegral steps
    loop stepsRemaining xC tC
      | stepsRemaining == 0 = xC
      | otherwise =
        loop (stepsRemaining - 1) (xC +. stepSize *: eqn xC tC) (tC + stepSize)

rungeKutta
    :: (VectorSpace a, Scalar a ~ Double)
    => Int
    -> (a -> Double -> a)
    -> a
    -> Double
    -> a
rungeKutta steps eqn x0 t
  | steps <= 0 = x0
  | otherwise = loop steps x0 0
  where
    stepSize = t / fromIntegral steps
    stepSizeHalf = 0.5 * stepSize
    stepSizeSixth = (1/6) * stepSize
    loop stepsRemaining xC tC
      | stepsRemaining == 0 = xC
      | otherwise =
        loop (stepsRemaining - 1)
            (xC +. stepSizeSixth*:(k1 +. 2*:(k2+.k3) +. k4))
            (tC + stepSize)
      where
        middleT = t + stepSizeHalf
        k1 = eqn xC t
        k2 = eqn (xC +. stepSizeHalf*:k1) middleT
        k3 = eqn (xC +. stepSizeHalf*:k2) middleT
        k4 = eqn (xC +. stepSize*:k3) (t + stepSize)
