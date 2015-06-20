
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

family :: CharField
family =
    CharField
        { λ = \x -> x M.! (1,1)
        , r = \_ -> 1
        , rarefactionCurve = \pt a -> pt + col [a]
        , shockCurve = \pt a -> pt + col [a]
        , shockSpeed = \pt a -> pt M.! (1,1) + a
        , gnl = True
        }

system :: System
system =
    System
        { n = 1
        , flux = \u -> (1/2) *. u^2
        , dFlux = id
        , families = [family]
        }

solution1 :: WaveFan
solution1 =
  fromJust $ waveFanFromList
      [ (Constant $ col [1], 1/2, Shock $ Just 1)
      , (Constant $ col [0], 0, Kink)
      ]

-- this is not an entropy solution
solution2 :: WaveFan
solution2 =
  fromJust $ waveFanFromList
      [ (Constant $ col [0], 1/2, Shock $ Just 1)
      , (Constant $ col [1], 0, Kink)
      ]

solution3 :: WaveFan
solution3 =
  fromJust $ waveFanFromList
      [ (Constant $ col [0], 0, Kink)
      , (Rarefaction (\x -> col [x]) $ Just 1, 1, Kink)
      , (Constant $ col [1], 0, Kink)
      ]

solutions = [solution1, solution2, solution3]

nonsolution :: WaveFan
nonsolution =
  fromJust $ waveFanFromList
      [ (Constant $ col [1], 1/2, Shock $ Just 1)
      , (Constant $ col [2], 0, Kink)
      ]

