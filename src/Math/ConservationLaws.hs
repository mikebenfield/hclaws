
module Math.ConservationLaws (
    matBpc,
    CharField(..), System(..), ValueWave(..), SeparatorWave(..), WaveFan(..),
    solutionForm,
    evalWaveAtSpeed, evalWaveFanAtSpeed, waveFanFromList,
    solveRiemann,
) where

import qualified Data.Matrix as M

import Math.LinearAlgebra
import qualified Math.Integration as I

type MatField = Mat -> Mat
type ScalarField = Mat -> Double
type Curve = Double -> Mat
type BasePointCurve = Mat -> Curve
type PotentialSolution = (Double, Double) -> Mat

matBpc :: (Double -> Double -> Double) -> BasePointCurve
matBpc f mx a = m (flip f a) mx

data CharField =
    CharField
        { λ :: ScalarField
        , r :: MatField
        , rarefactionCurve :: BasePointCurve
        , shockCurve :: BasePointCurve
        , shockSpeed :: Mat -> Double -> Double
        , gnl :: Bool
        }

data System =
    System
        { n :: Int
        , flux :: MatField
        , dFlux :: MatField
        , families :: [CharField]
        }

data ValueWave =
    Rarefaction (Double -> Mat) (Maybe Int)
  | Constant Mat

evalWaveAtSpeed :: ValueWave -> Double -> Mat
evalWaveAtSpeed (Rarefaction f _) speed = f speed
evalWaveAtSpeed (Constant v) _ = v

data SeparatorWave =
    Shock (Maybe Int)
  | Front (Maybe Int)
  | Kink

data WaveFan =
    Waves
        { value :: ValueWave
        , endSpeed :: Double
        , endingWave :: SeparatorWave
        , rest :: WaveFan
        }
  | Wave ValueWave

findWaveAtSpeed :: WaveFan -> Double -> ValueWave
findWaveAtSpeed Waves {value=w, endSpeed=es, rest=r} s
  | s <= es = w
  | otherwise = findWaveAtSpeed r s
findWaveAtSpeed (Wave w) _ = w

evalWaveFanAtSpeed :: WaveFan -> Double -> Mat
evalWaveFanAtSpeed wf s =
    evalWaveAtSpeed (findWaveAtSpeed wf s) s

evalWaveFanAtPoint :: WaveFan -> Double -> Double -> Mat
evalWaveFanAtPoint wf x t = evalWaveFanAtSpeed wf (x/t)

solveRiemann :: System -> Double -> Double -> WaveFan
solveRiemann sys uL uR = error "XXX"

waveFanFromList :: [(ValueWave, Double, SeparatorWave)] -> Maybe WaveFan
waveFanFromList [] = Nothing
waveFanFromList ((vw, _, _):[])         = Just $ Wave vw
waveFanFromList ((vw, speed, sep):more) =
    case waveFanFromList more of
        Nothing -> Nothing
        Just rest' ->
            Just $ Waves
                { value = vw
                , endSpeed = speed
                , endingWave = sep
                , rest = rest'
                }

solutionForm :: System -> PotentialSolution -> (Double, Double) -> Mat
solutionForm s u (x, t) = u (x, t) M.<|> negate (flux s $ u (x, t))

accuracy = 0.01
epsilon = 0.01

checkSolnOnCurve
    :: (Double -> (Double, Double))
    -> (Double -> Mat)
    -> System
    -> PotentialSolution
    -> Mat
checkSolnOnCurve c c' s u =
  I.adaptiveSimpsonLineIntegral accuracy c c' (solutionForm s u) 0 1



