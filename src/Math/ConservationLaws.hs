{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Math.ConservationLaws (
    Vector,
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
import Control.Monad.ST

import GHC.TypeLits

import qualified Data.Vector as V

import qualified Math.FTensor.General as F
import qualified Math.FTensor.Lib.Array as A

import Math.LinearAlgebra
import Math.Fan
import qualified Math.Curves as C
import qualified Math.Integration as I

type Vector (dim::Nat) = F.TensorBoxed '[dim] Double

type VectorField (dim::Nat) = Vector dim -> Vector dim

type ScalarField (dim::Nat) = Vector dim -> Double

type Curve (dim::Nat) = Double -> Vector dim

type BasePointCurve (dim::Nat) = Vector dim -> Curve dim

type PotentialSolution (dim::Nat) = Point -> Vector dim

data Linearity = GNL | LDG | Neither
    deriving (Eq, Show)

data CharField (n::Nat) =
    CharField
        { λ :: ScalarField n
        , r :: VectorField n
        , rarefactionCurve :: BasePointCurve n
        , shockCurve :: BasePointCurve n
        , shockSpeed :: Vector n -> Vector n -> Double
        , linearity :: Linearity
        }

data System (n::Nat) =
    System
        { flux :: VectorField n
        , dFlux :: Vector n -> F.TensorBoxed '[n, n] Double
        , fields :: [CharField n]
        , solveRiemann :: Vector n -> Vector n -> WaveFan n
        }

data Rarefaction (n::Nat) =
    Rarefaction
        { speedL :: Double
        , speedR :: Double
        , function :: Double -> Vector n
        , rFamily :: Int
        }

instance Show (Rarefaction n) where
    show Rarefaction{..} =
        "Rarefaction {speedL = " ++ show speedL ++
            ", speedR = " ++ show speedR ++
            ", rFamily = " ++ show rFamily ++ "}"

data Shock =
    Shock
        { speed :: Double
        , sFamily :: Int
        } deriving Show

data Wave (n::Nat) = RWave (Rarefaction n) | SWave Shock
    deriving Show

fastestSpeed :: Wave n -> Double
fastestSpeed (RWave Rarefaction{..}) = speedR
fastestSpeed (SWave Shock{..}) = speed

rarefactionWave :: CharField n -> Int -> Vector n -> Vector n -> Wave n
rarefactionWave field familyI uL uR =
    RWave Rarefaction
        { speedL = λ field uL
        , speedR = λ field uR
        , function = \s -> rarefactionCurve field uL (s - λ field uL)
        , rFamily = familyI
        }

shockWave :: CharField n -> Int -> Vector n -> Vector n -> Wave n
shockWave field familyI uL uR =
    SWave Shock {speed = shockSpeed field uL uR, sFamily = familyI}

type WaveFan (n::Nat) = Fan (Vector n) (Wave n)

-- can't use findOuterAt, because we don't need just the value
atSpeed :: WaveFan n -> Double -> Vector n
atSpeed (Fan v last) speed =
    case V.find (\(_, wave) -> speed <= fastestSpeed wave) v of
        Nothing -> last
        Just (_, RWave Rarefaction{..})
            | speed > speedL -> function speed
        Just (value, _) -> value

atPoint :: WaveFan n -> Point -> Vector n
atPoint wf Point{..} = atSpeed wf (x/t)

solutionForm
    :: System n
    -> PotentialSolution n
    -> Point
    -> F.TensorBoxed '[n, 2] Double
solutionForm s u pt = F.Tensor $ runST $ do
    newArr <- A.new (2*len)
    let writeOne j =
            let idx = 2*j
            in
            A.write newArr idx (A.index uptArr j) >>
                A.write newArr (idx+1) (- A.index fuptArr j)
        loop j
          | j >= len = return ()
          | otherwise = writeOne j >> loop (j+1)
    loop 0
    A.freeze newArr
  where
    len = A.length uptArr
    upt@(F.Tensor uptArr) = u pt
    F.Tensor fuptArr = flux s upt

accuracy = 0.000001

integrateFanOnCurve :: KnownNat n => C.Curve -> System n -> WaveFan n -> Vector n
integrateFanOnCurve c s wf =
    I.adaptiveSimpsonLineIntegral accuracy c solutionForm' 0 1
  where
    solutionForm' = solutionForm s $ atPoint wf

strengthsToFan :: Vector n -> [CharField n] -> [Double] -> WaveFan n
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
