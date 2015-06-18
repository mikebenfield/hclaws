
module Math.ConservationLaws (
  CharField(..), System(..), ValueWave, SeparatorWave, WaveFan,
  solutionForm, 
  evalWaveAtSpeed, evalWaveFanAtSpeed,
  solveRiemann,
) where

import qualified Data.Matrix as M

import Math.LinearAlgebra

type MatField = Mat -> Mat
type ScalarField = Mat -> Double
type Curve = Double -> Mat
type BasePointCurve = Mat -> Curve

data CharField = CharField { λ                :: ScalarField
                           , r                :: MatField
                           , rarefactionCurve :: BasePointCurve
                           , shockCurve       :: BasePointCurve
                           , s                :: Mat -> Double -> Double
                           , gnl              :: Bool
                           }

data System = System { n        :: Int
                     , flux     :: MatField
                     , dFlux    :: MatField
                     , families :: [CharField]
                     }

solutionForm :: System -> MatField
solutionForm s u = M.matrix (n s) 2 f
  where
    f (i, 1) = u M.! (i, 1)
    f (i, 2) = -(flux s u) M.! (i, 1)
    f _ = error "solutionForm"

--differentialForm :: VecField -> VecField -> VecField
--differentialForm u flux' x = V.fromList [ux, flux' ux]
--  where
--    ux = u x

-- The argument in the case of Kink or Shock is the speed of the wave
data ValueWave = Rarefaction (Double -> Mat) (Maybe Int)
               | Constant Mat

evalWaveAtSpeed :: ValueWave -> Double -> Mat
evalWaveAtSpeed (Rarefaction f _) speed = f speed
evalWaveAtSpeed (Constant v) _          = v

data SeparatorWave = Shock (Maybe Int)
                   | Front (Maybe Int)
                   | Kink 

data WaveFan = Waves { value      :: ValueWave
                     , endSpeed   :: Double
                     , endingWave :: SeparatorWave
                     , rest       :: WaveFan
                     }
             | Wave ValueWave

findWaveAtSpeed :: WaveFan -> Double -> ValueWave
findWaveAtSpeed Waves {value=w, endSpeed=es, rest=r} s =
  if s <= es then
    w
  else
    findWaveAtSpeed r s
findWaveAtSpeed (Wave w) _ = w
    
evalWaveFanAtSpeed :: WaveFan -> Double -> Mat
evalWaveFanAtSpeed wf s =
  evalWaveAtSpeed (findWaveAtSpeed wf s) s
  
evalWaveFanAtPoint :: WaveFan -> Double -> Double -> Mat
evalWaveFanAtPoint wf x t = evalWaveFanAtSpeed wf (x/t)

solveRiemann :: System -> Double -> Double -> WaveFan
solveRiemann sys uL uR = error "XXX"

