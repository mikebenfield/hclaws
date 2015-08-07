
module Math.ConservationLaws.Examples.Linear (
    system,
) where

import qualified Data.Matrix as M

import Math.LinearAlgebra
import Math.ConservationLaws

field1 :: CharField
field1 =
    CharField
        { λ = \_ -> -1
        , r = \_ -> col [1, 0, 0]
        , rarefactionCurve = \pt a -> pt + col [a, 0, 0]
        , shockCurve = \pt a -> pt + col [a, 0, 0]
        , shockSpeed = \_ _ -> -1
        , linearity = LDG
        }

field2 :: CharField
field2 =
    CharField
        { λ = \_ -> 0
        , r = \_ -> col [0, 1, 0]
        , rarefactionCurve = \pt a -> pt + col [0, a, 0]
        , shockCurve = \pt a -> pt + col [0, a, 0]
        , shockSpeed = \_ _ -> 0
        , linearity = LDG
        }

field3 :: CharField
field3 =
    CharField
        { λ = \_ -> 1
        , r = \_ -> col [0, 0, 1]
        , rarefactionCurve = \pt a -> pt + col [0, 0, a]
        , shockCurve = \pt a -> pt + col [0, 0, a]
        , shockSpeed = \_ _ -> 1
        , linearity = LDG
        }

dFlux' :: M.Matrix Double
dFlux' = M.fromLists [[1, 0, 0], [0, 0, 0], [0, 0, 1]]

one = (M.! (1, 1))
two = (M.! (2, 1))
three = (M.! (3, 1))

system :: System
system =
    System
        { n = 3
        , flux = \u -> col [-1 * one u, 0, three u]
        , dFlux = \_ -> dFlux'
        , fields = [field1, field2, field3]
        , solveRiemann = \ul ur ->
            strengthsToFan ul (fields system)
                [one ur - one ul, two ur - two ul, three ur - three ul]
        }
