
module Math.ConservationLaws.Examples (
    burgers,
    burgersSolution1, burgersSolution2, burgersSolution3,
    burgersNonsolution,
) where

import Data.Maybe (fromJust)

import qualified Data.Matrix as M

import Math.LinearAlgebra
import Math.ConservationLaws

burgersFamily :: CharField
burgersFamily =
    CharField
        { λ = \x -> x M.! (1,1)
        , r = \_ -> 1
        , rarefactionCurve = \pt a -> pt + col [a]
        , shockCurve = \pt a -> pt + col [a]
        , shockSpeed = \pt a -> pt M.! (1,1) + a
        , gnl = True
        }

burgers :: System
burgers =
    System
        { n = 1
        , flux = \u -> (1/2) *. u^2
        , dFlux = id
        , families = [burgersFamily]
        }

burgersSolution1 :: WaveFan
burgersSolution1 =
  fromJust $ waveFanFromList
      [ (Constant $ col [1], 1/2, Shock $ Just 1)
      , (Constant $ col [0], 0, Kink)
      ]

-- this is not an entropy solution
burgersSolution2 :: WaveFan
burgersSolution2 =
  fromJust $ waveFanFromList
      [ (Constant $ col [0], 1/2, Shock $ Just 1)
      , (Constant $ col [1], 0, Kink)
      ]

burgersSolution3 :: WaveFan
burgersSolution3 =
  fromJust $ waveFanFromList
      [ (Constant $ col [0], 0, Kink)
      , (Rarefaction (\x -> col [x]) $ Just 1, 1, Kink)
      , (Constant $ col [1], 0, Kink)
      ]

burgersNonsolution :: WaveFan
burgersNonsolution =
  fromJust $ waveFanFromList
      [ (Constant $ col [1], 1/2, Shock $ Just 1)
      , (Constant $ col [2], 0, Kink)
      ]

