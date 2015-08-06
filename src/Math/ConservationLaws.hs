
module Math.ConservationLaws (
    CharField(..),
    System(..),
    Wave(..),
    WaveFan,
    Rarefaction(..),
    Shock(..),
    Linearity(..),
    rarefactionWave,
    shockWave,
    solutionForm,
    atSpeed,
    atPoint,
    integrateFanOnCurve,
    strengthsToFan,
) where

import Prelude hiding (length)

import qualified Data.Matrix as M
import qualified Data.Vector as V

import Math.LinearAlgebra
import Math.Fan
import qualified Math.Curves as C
import qualified Math.Integration as I

type MatField = Mat -> Mat
type ScalarField = Mat -> Double
type Curve = Double -> Mat
type BasePointCurve = Mat -> Curve
type PotentialSolution = Double -> Double -> Mat

data Linearity = GNL | LDG | Neither
    deriving (Eq, Show)

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

data Rarefaction =
    Rarefaction
        { speedL :: Double
        , speedR :: Double
        , function :: Double -> Mat
        , rFamily :: Int
        }

instance Show Rarefaction where
    show Rarefaction{..} =
        "Rarefaction {speedL = " ++ show speedL ++
            ", speedR = " ++ show speedR ++
            ", rFamily = " ++ show rFamily ++ "}"

data Shock =
    Shock
        { speed :: Double
        , sFamily :: Int
        } deriving Show

data Wave = RWave Rarefaction | SWave Shock
    deriving Show

fastestSpeed :: Wave -> Double
fastestSpeed (RWave Rarefaction{..}) = speedR
fastestSpeed (SWave Shock{..}) = speed

rarefactionWave :: CharField -> Int -> Mat -> Mat -> Wave
rarefactionWave field familyI uL uR =
    RWave Rarefaction
        { speedL = λ field uL
        , speedR = λ field uR
        , function = \s -> rarefactionCurve field uL (s - λ field uL)
        , rFamily = familyI
        }

shockWave :: CharField -> Int -> Mat -> Mat -> Wave
shockWave field familyI uL uR =
    SWave Shock {speed = shockSpeed field uL uR, sFamily = familyI}

type WaveFan = Fan Mat Wave

-- can't use findOuterAt, because we don't need just the value
atSpeed :: WaveFan -> Double -> Mat
atSpeed (Fan v last) speed =
    case V.find (\(_, wave) -> speed <= fastestSpeed wave) v of
        Nothing -> last
        Just (_, RWave Rarefaction{..})
            | speed > speedL -> function speed
        Just (value, _) -> value

atPoint :: WaveFan -> Double -> Double -> Mat
atPoint wf x t = atSpeed wf (x/t)

solutionForm :: System -> PotentialSolution -> Mat -> Mat
solutionForm s u xt =
    uxt M.<|> negate (flux s uxt)
  where
    x = xt M.! (1, 1)
    t = xt M.! (2, 1)
    uxt = u x t

accuracy = 0.000001

integrateFanOnCurve :: C.Curve -> System -> WaveFan -> Mat
integrateFanOnCurve c s wf =
    I.adaptiveSimpsonLineIntegral accuracy c solutionForm' 0 1
  where
    solutionForm' =
        solutionForm s $ atPoint wf

strengthsToFan :: Mat -> [CharField] -> [Double] -> WaveFan
strengthsToFan uL fields strengths =
    Fan (V.generate (V.length shifted) unshift) (fst $ V.last shifted)
  where
    shifted = V.unfoldr strengthsToFan_ (uL, fields, strengths, [0..])
    unshift 0 = (uL, snd $ V.unsafeIndex shifted 0)
    unshift i =
        (fst $ V.unsafeIndex shifted (i-1), snd $ V.unsafeIndex shifted i)

strengthsToFan_ (_, [], [], _) = Nothing
strengthsToFan_ (uL, _:fs, 0:ss, _:is) = strengthsToFan_ (uL, fs, ss, is)
strengthsToFan_ (uL, f:fs, s:ss, i:is) =
    if linearity f == LDG || s < 0 then
        Just
            ( (u2Shock, shockWave f i uL u2Shock)
            , (u2Shock, fs, ss, is)
            )
    else
        Just
            ( (u2Rare, rarefactionWave f i uL u2Rare)
            , (u2Rare, fs, ss, is)
            )
  where
    u2Rare = rarefactionCurve f uL s
    u2Shock = shockCurve f uL s
strengthsToFan_ _ = error "strengthsToFan"
