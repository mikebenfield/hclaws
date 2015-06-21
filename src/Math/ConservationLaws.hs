
module Math.ConservationLaws (
    CharField(..), System(..), Wave(..), WaveFan(..),
    Linearity(..),
    solutionForm,
    atSpeed, atPoint,
    solveRiemann,
    checkSolnOnCurve,
) where

import qualified Data.Matrix as M

import Math.LinearAlgebra
import qualified Math.Integration as I

type MatField = Mat -> Mat
type ScalarField = Mat -> Double
type Curve = Double -> Mat
type BasePointCurve = Mat -> Curve
type PotentialSolution = Double -> Double -> Mat

data Linearity = GNL | LDG | Neither

data CharField =
    CharField
        { λ :: ScalarField
        , r :: MatField
        , rarefactionCurve :: BasePointCurve
        , shockCurve :: BasePointCurve
        , shockSpeed :: Mat -> Mat -> Double
        , linearity :: Linearity
        }

data System =
    System
        { n :: Int
        , flux :: MatField
        , dFlux :: MatField
        , fields :: [CharField]
        , strengths :: Mat -> Mat -> Mat
        }

data Wave =
    Rarefaction Double Double (Double -> Mat) (Maybe Int)
  | Shock Double (Maybe Int)
  | Front Double (Maybe Int)

startSpeed :: Wave -> Double
startSpeed (Rarefaction s _ _ _) = s
startSpeed (Shock s _) = s
startSpeed (Front s _) = s

endSpeed :: Wave -> Double
endSpeed (Rarefaction _ s _ _) = s
endSpeed (Shock s _) = s
endSpeed (Front s _) = s

data WaveFan =
    Waves Mat Wave WaveFan
  | Last Mat

atSpeed :: WaveFan -> Double -> Mat
atSpeed (Waves m (Rarefaction startSpeed_ endSpeed_ f _) wf) s
  | s <= startSpeed_ = m
  | s <= endSpeed_ = f s
  | otherwise = atSpeed wf s
atSpeed (Waves m w wf) s
  | s <= startSpeed w = m
  | otherwise = atSpeed wf s
atSpeed (Last m) _ = m

atPoint :: WaveFan -> Double -> Double -> Mat
atPoint wf x t = atSpeed wf (x/t)

solutionForm :: System -> PotentialSolution -> Mat -> Mat
solutionForm s u xt =
    uxt M.<|> negate (flux s $ uxt)
  where
    x = xt M.! (1, 1)
    t = xt M.! (2, 1)
    uxt = u x t

accuracy = 0.000001

checkSolnOnCurve
    :: Curve
    -> Curve
    -> System
    -> PotentialSolution
    -> Mat
checkSolnOnCurve c c' s u =
    I.adaptiveSimpsonLineIntegral accuracy c c' (solutionForm s u) 0 1

solveRiemann :: System -> Double -> Double -> WaveFan
solveRiemann sys uL uR = error "XXX"

