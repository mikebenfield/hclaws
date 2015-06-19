
module Math.ConservationLaws.Examples (
  --burgers

) where

import Data.Maybe (fromJust)

import qualified Data.Matrix as M

import Math.LinearAlgebra
import Math.ConservationLaws 

burgersFamily :: CharField Double
burgersFamily = CharField { λ                     = id
                          , r                     = \_ -> 1
                          , rarefactionCurve      = \pt a -> pt + a
                          , shockCurve            = \pt a -> pt + a
                          , s                     = \pt a -> pt + a
                          , gnl                   = True
                          }

burgers :: System Double
burgers = System { n        = 1
                 , flux     = \u -> u^2/2
                 , dFlux    = id
                 , families = [burgersFamily]
                 }

burgersSolution1 :: WaveFan Double
burgersSolution1 = 
  fromJust $ waveFanFromList [(Constant 1, 1/2, Shock $ Just 1),
                              (Constant 0, 0, Kink)]
  
-- this is not an entropy solution
burgersSolution2 :: WaveFan Double
burgersSolution2 = 
  fromJust $ waveFanFromList [(Constant 0, 1/2, Shock $ Just 1),
                              (Constant 1, 0, Kink)]

burgersSolution3 :: WaveFan Double
burgersSolution3 =
  fromJust $ waveFanFromList [(Constant 0, 0, Kink),
                              (Rarefaction id, 1, Kink),
                              (Constant 1, 0, Kink)]

