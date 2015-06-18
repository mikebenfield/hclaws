
module Math.ConservationLaws.Examples (
  --burgers

) where

import qualified Data.Matrix as M

import Math.LinearAlgebra
import Math.ConservationLaws 

--burgersFamily = CharField { λ                     = \m -> m M.! (1, 1)
--                          , r                     = \_ -> col [1]
--                          , rarefactionCurve      = \pt a -> col [pt M.! (1,1) + a]
--                          , shockCurve            = \pt a -> col [pt M.! (1,1) + a]
--                          , s                     = \pt a -> pt M.! (1,1) + a
--                          , gnl                   = True
--                          }
--
--burgers :: System
--burgers = System { n        = 1
--                 , flux     = \u -> col [(u M.! (1,1))^2/2]
--                 , dFlux    = \u -> col [u M.! (1,1)]
--                 , families = [burgersFamily]
--                 }
--
--
--burgersSolution1 = Waves { value      = Constant (col [1])
--                         , endSpeed   = 1/2
--                         , endingWave = Shock 1
--                         , rest       = Wave (Constant (col [0]))
--                         }
--
--burgersSolution2 =
--  Waves { value      = Constant (col [0])
--        , endSpeed   = 0
--        , endingWave = Kink
--        , rest       = Waves { value      = Rarefaction (\u -> u M.! (1,1)) 1
--                             , endSpeed   = 1
--                             , endingWave = Kink
--                             , rest       = Wave (Constant (col [1]))
--                             }
--        }


