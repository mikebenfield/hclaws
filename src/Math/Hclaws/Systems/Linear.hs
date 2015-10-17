{-# LANGUAGE DataKinds #-}

module Math.Hclaws.Systems.Linear (
    system,
) where

import Data.Proxy

import Math.FTensor.Algebra
import qualified Math.FTensor.General as F

import Math.Hclaws.ConservationLaws

field1 :: CharField 3
field1 =
    CharField
        { λ = \_ -> -1
        , r = \_ -> [1, 0, 0]
        , rarefactionCurve = \pt a -> pt +. [a, 0, 0]
        , shockCurve = \pt a -> pt +. [a, 0, 0]
        , shockSpeed = \_ _ -> -1
        , linearity = LDG
        }

field2 :: CharField 3
field2 =
    CharField
        { λ = \_ -> 0
        , r = \_ -> [0, 1, 0]
        , rarefactionCurve = \pt a -> pt +. [0, a, 0]
        , shockCurve = \pt a -> pt +. [0, a, 0]
        , shockSpeed = \_ _ -> 0
        , linearity = LDG
        }

field3 :: CharField 3
field3 =
    CharField
        { λ = \_ -> 1
        , r = \_ -> [0, 0, 1]
        , rarefactionCurve = \pt a -> pt +. [0, 0, a]
        , shockCurve = \pt a -> pt +. [0, 0, a]
        , shockSpeed = \_ _ -> 1
        , linearity = LDG
        }

dFlux' :: F.TensorBoxed '[3, 3] Double
dFlux' = [[1, 0, 0], [0, 0, 0], [0, 0, 1]]

a :: F.TensorBoxed '[3] Double -> Double
a t = F.pIndex t (Proxy::Proxy '[0])
b :: F.TensorBoxed '[3] Double -> Double
b t = F.pIndex t (Proxy::Proxy '[1])
c :: F.TensorBoxed '[3] Double -> Double
c t = F.pIndex t (Proxy::Proxy '[2])

system :: System 3
system =
    System
        { flux = \u -> [-1 * a u, 0, c u]
        , dFlux = \_ -> dFlux'
        , fields = [field1, field2, field3]
        , solveRiemann = \ul ur ->
            strengthsToFan ul (fields system)
                [a ur - a ul, b ur - b ul, c ur - c ul]
        }
