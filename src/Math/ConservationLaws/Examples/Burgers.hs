{-# LANGUAGE DataKinds #-}

module Math.ConservationLaws.Examples.Burgers (
    system,
    solution1, solution2, solution3, solution4, solution5,
    solutions,
    nonsolution,
) where

import Data.Proxy

import qualified Math.FTensor.General as F
import Math.FTensor.Algebra

import Math.Fan
import Math.ConservationLaws

u :: F.TensorBoxed '[1] Double -> Double
u pt = F.pIndex pt (Proxy::Proxy '[0])

box :: Double -> F.TensorBoxed '[1] Double
box v = F.Tensor [v]

field :: CharField 1
field =
    CharField
        { λ = u
        , r = \_ -> box 1
        , rarefactionCurve = \pt a -> box (u pt +. a)
        , shockCurve = \pt a -> box (u pt +. a)
        , shockSpeed = \ul ur -> 0.5 * (u ul + u ur)
        , linearity = GNL
        }

system :: System 1
system =
    System
        { flux = \v -> box (0.5 * (u v)^(2::Int))
        , dFlux = \(F.Tensor arr) -> F.Tensor arr
        , fields = [field]
        , solveRiemann = \ul ur -> strengthsToFan ul [field] [u ur - u ul]
        }

solution1 :: WaveFan 1
solution1 =
    Fan [(box 1, SWave Shock {speed = 0.5, sFamily = 0})] (box 1)

-- this is not an entropy solution
solution2 :: WaveFan 1
solution2 =
    Fan [(box 0, SWave Shock {speed = 0.5, sFamily = 0})] (box 1)

solution3 :: WaveFan 1
solution3 =
    Fan [(box 0, wave)] (box 1)
  where
    wave =
        RWave Rarefaction
            { speedL = 0
            , speedR = 1
            , function = box
            , rFamily = 0
            }

solution4 = solveRiemann system (box 1) (box 4)

solution5 = solveRiemann system (box 1) (box (-2))

solutions :: [WaveFan 1]
solutions = [solution1, solution2, solution3, solution4, solution5]

nonsolution :: WaveFan 1
nonsolution =
    Fan [(box 1, SWave Shock {speed = 0.5, sFamily = 0})] (box 2)
