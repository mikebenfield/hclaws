
module Math.ConservationLaws.Examples.Burgers (
    system,
    solution1, solution2, solution3,
    solutions,
    nonsolution,
) where

import Data.Maybe (fromJust)

import qualified Data.Matrix as M

import Math.LinearAlgebra
import Math.ConservationLaws

u pt = pt M.! (1,1)

field :: CharField
field =
    CharField
        { λ = \x -> x M.! (1,1)
        , r = \_ -> 1
        , rarefactionCurve = \pt a -> pt + col [a]
        , shockCurve = \pt a -> pt + col [a]
        , shockSpeed = \ul ur -> (1/2) * ((ul + ur) M.! (1,1))
        , linearity = GNL
        }

system :: System
system =
    System
        { n = 1
        , flux = \u -> (1/2) *. u^2
        , dFlux = id
        , fields = [field]
        , solveRiemann = \ul ur -> strengthsToFan ul [field] [u $ ur - ul]
        }

solution1 :: WaveFan
solution1 =
    Waves (col [1]) (Shock 0.5 $ Just 1) $
    Last (col [0])

-- this is not an entropy solution
solution2 :: WaveFan
solution2 =
    Waves (col [0]) (Shock 0.5 $ Just 1) $
    Last (col [1])

solution3 :: WaveFan
solution3 =
    Waves (col [0]) (Rarefaction 0 1 (\x -> col [x]) $ Just 1) $
    Last (col [1])

solutions = [solution1, solution2, solution3]

nonsolution :: WaveFan
nonsolution =
    Waves (col [1]) (Shock 0.5 $ Just 1) $
    Last (col [2])

