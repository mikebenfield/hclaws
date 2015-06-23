
module Math.ConservationLaws (
    CharField(..), System(..), Wave(..), WaveFan(..),
    Linearity(..),
    rarefactionWave, shockWave,
    solutionForm,
    atSpeed, atPoint,
    integrateFanOnCurve,
    strengthsToFan,
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
        , solveRiemann :: Mat -> Mat -> WaveFan
        }

data Wave =
    Rarefaction Double Double (Double -> Mat) (Maybe Int)
  | Shock Double (Maybe Int)
  | Front Double (Maybe Int)

instance Show Wave where
    show (Rarefaction a b _ (Just i)) =
        "Rarefaction " ++ show a ++ " " ++ show b ++ " " ++ show i
    show (Shock a (Just i)) =
        "Shock " ++ show a ++ " " ++ show i
    show _ = error "Can't happen"

rarefactionWave :: CharField -> Int-> Mat -> Mat -> Wave
rarefactionWave f i uL uR =
    Rarefaction λuL (λ f uR) (\a -> rarefactionCurve f uL (a - λuL)) $ Just i
  where
    λuL = λ f uL

shockWave :: CharField -> Int-> Mat -> Mat -> Wave
shockWave f i uL uR =
    Shock (shockSpeed f uL uR) $ Just i

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
  deriving Show

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

integrateFanOnCurve :: (Curve, Curve) -> System -> WaveFan -> Mat
integrateFanOnCurve (c, c') s wf =
    I.adaptiveSimpsonLineIntegral accuracy c c' solutionForm' 0 1
  where
    solutionForm' =
        solutionForm s $ atPoint wf

strengthsToFan' :: Mat -> [CharField] -> [Double] -> [Int] -> WaveFan
strengthsToFan' u1 [] [] _ = Last u1
strengthsToFan' _ [] _ _ = error "strengthsToFan: more strengths than fields"
strengthsToFan' _ _ [] _ = error "strengthsToFan: more fields than strengths"
strengthsToFan' _ _ _ [] = error "strengthsToFan: ???"
strengthsToFan' u1 (f:fs) (0:ss) (i:is) = strengthsToFan' u1 fs ss is
strengthsToFan' u1 (f:fs) (s:ss) (i:is) =
    case linearity f of
        LDG ->
            Waves u1 (Shock (λ f u1) $ Just i) $
                strengthsToFan' u2Rare fs ss is
        GNL
          | s > 0 ->
                Waves u1 (Rarefaction (λ f u1) (λ f u2Rare) rCurveλ (Just i)) $
                    strengthsToFan' u2Rare fs ss is
          | otherwise ->
                Waves u1 (Shock sSpeed $ Just i) $
                    strengthsToFan' u2Shock fs ss is
        _ -> error $ "strengthsToFan: " ++ show i ++
                 "th field neither LDG nor GNL"
  where
    rCurve = rarefactionCurve f u1
    sCurve = shockCurve f u1
    u2Rare = rCurve s
    u2Shock = sCurve s
    sSpeed = shockSpeed f u1 u2Shock
    rCurveλ λ' = rCurve (λ' - λ f u1)

strengthsToFan :: Mat -> [CharField] -> [Double] -> WaveFan
strengthsToFan u1 fs ss = strengthsToFan' u1 fs ss [1..]

