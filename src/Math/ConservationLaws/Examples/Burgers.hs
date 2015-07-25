
module Math.ConservationLaws.Examples.Burgers (
    system,
    solution1, solution2, solution3, solution4, solution5,
    solutions,
    nonsolution,
) where

import Data.Maybe (fromJust)

import qualified Data.Matrix as M

import Math.Fan
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
    Fan [(1, SWave Shock {speed = 0.5, sFamily = 0})] 0

-- this is not an entropy solution
solution2 :: WaveFan
solution2 =
    Fan [(0, SWave Shock {speed = 0.5, sFamily = 0})] 1

solution3 :: WaveFan
solution3 =
    Fan [(0, wave)] 1
  where
    wave =
        RWave Rarefaction
            { speedL = 0
            , speedR = 1
            , function = (\x -> col [x])
            , rFamily = 0
            }

solution4 = solveRiemann system 1 4

solution5 = solveRiemann system 1 (-2)

solutions :: [WaveFan]
solutions = [solution1, solution2, solution3, solution4, solution5]

nonsolution :: WaveFan
nonsolution =
    Fan [(1, SWave Shock {speed = 0.5, sFamily = 0})] 2

