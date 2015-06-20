
module Math.ConservationLaws.Examples (
    burgers
) where

import Data.Maybe (fromJust)

import qualified Data.Matrix as M

import Math.LinearAlgebra
import Math.ConservationLaws

burgersFamily :: CharField
burgersFamily =
    CharField
        { λ = \x -> x M.! (1,1)
        , r = m $ \_ -> 1
        , rarefactionCurve = matBpc $ \pt a -> pt + a
        , shockCurve = matBpc $ \pt a -> pt + a
        , shockSpeed = \pt a -> pt M.! (1,1) + a
        , gnl = True
        }

burgers :: System
burgers =
    System
        { n = 1
        , flux = m $ \u -> u^2/2
        , dFlux = m id
        , families = [burgersFamily]
        }

burgersSolution1 :: WaveFan
burgersSolution1 =
  fromJust $ waveFanFromList
      [ (Constant $ col [1], 1/2, Shock $ Just 1)
      , (Constant $ 0, 0, Kink)
      ]

-- this is not an entropy solution
burgersSolution2 :: WaveFan
burgersSolution2 =
  fromJust $ waveFanFromList
      [ (Constant $ col [1], 1/2, Shock $ Just 1)
      , (Constant $ col [1], 0, Kink)
      ]

burgersSolution3 :: WaveFan
burgersSolution3 =
  fromJust $ waveFanFromList 
      [ (Constant $ col [1], 0, Kink)
      , (Rarefaction (\x -> col [x]) $ Just 1, 1, Kink)
      , (Constant $ col [1], 0, Kink)
      ]

