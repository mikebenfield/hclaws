
module Math.ConservationLaws (
  CharField(..), System(..), ValueWave(..), SeparatorWave(..), WaveFan(..),
  solutionForm, 
  evalWaveAtSpeed, evalWaveFanAtSpeed, waveFanFromList,
  solveRiemann,
) where

import qualified Data.Matrix as M

import Math.LinearAlgebra

type MatField a = a -> a
type ScalarField a = a -> Double
type Curve a = Double -> a
type BasePointCurve a = a -> Curve a
type PotentialSolution a = Double -> Double -> a

data CharField a = CharField { λ                :: ScalarField a
                             , r                :: MatField a
                             , rarefactionCurve :: BasePointCurve a
                             , shockCurve       :: BasePointCurve a
                             , s                :: a -> Double -> Double
                             , gnl              :: Bool
                             }

data System a = System { n        :: Int
                       , flux     :: MatField a
                       , dFlux    :: MatField a
                       , families :: [CharField a]
                       }

solutionForm :: Num a => System a 
             -> PotentialSolution a -> Double -> Double -> M.Matrix a
solutionForm s u x t = row [u x t, -(flux s $ u x t)]

data ValueWave a = Rarefaction (Double -> a) (Maybe Int)
                 | Constant a

evalWaveAtSpeed :: ValueWave a -> Double -> a
evalWaveAtSpeed (Rarefaction f _) speed = f speed
evalWaveAtSpeed (Constant v) _          = v

data SeparatorWave = Shock (Maybe Int)
                   | Front (Maybe Int)
                   | Kink 

data WaveFan a = Waves { value      :: ValueWave a
                       , endSpeed   :: Double
                       , endingWave :: SeparatorWave
                       , rest       :: WaveFan a
                       }
               | Wave (ValueWave a)

findWaveAtSpeed :: WaveFan a -> Double -> ValueWave a
findWaveAtSpeed Waves {value=w, endSpeed=es, rest=r} s =
  if s <= es then
    w
  else
    findWaveAtSpeed r s
findWaveAtSpeed (Wave w) _ = w
    
evalWaveFanAtSpeed :: WaveFan a -> Double -> a
evalWaveFanAtSpeed wf s =
  evalWaveAtSpeed (findWaveAtSpeed wf s) s
  
evalWaveFanAtPoint :: WaveFan a -> Double -> Double -> a
evalWaveFanAtPoint wf x t = evalWaveFanAtSpeed wf (x/t)

solveRiemann :: System a -> Double -> Double -> WaveFan a
solveRiemann sys uL uR = error "XXX"

waveFanFromList :: [(ValueWave a, Double, SeparatorWave)] -> Maybe (WaveFan a)
waveFanFromList [] = Nothing
waveFanFromList ((vw, _, _):[])         = Just $ Wave vw
waveFanFromList ((vw, speed, sep):more) = 
  case waveFanFromList more of
    Nothing -> Nothing
    Just rest' ->
      Just $ Waves { value = vw
                   , endSpeed = speed
                   , endingWave = sep
                   , rest = rest'
                   }

