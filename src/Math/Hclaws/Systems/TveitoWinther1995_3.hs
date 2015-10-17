{-# LANGUAGE DataKinds #-}

module Math.Hclaws.Systems.TveitoWinther1995_3 (
    system
) where

import Data.Proxy

import Math.FTensor.General as F

import Math.Hclaws.ConservationLaws
import Math.Hclaws.Fan

u :: Vector 2 -> Double
u t = F.pIndex t (Proxy::Proxy '[0])

v :: Vector 2 -> Double
v t = F.pIndex t (Proxy::Proxy '[1])

rarefaction1 pt a =
    [a, (v pt) * (u pt) * (1 - u pt) / (a * (1-a))]

field1 = CharField
    { λ = \_ -> 0
    , r = \t ->
        let u' = u t
        in
        [u' - u'^(2::Int), v t * (2*u' - 1)]
    , rarefactionCurve = rarefaction1
    , shockCurve = rarefaction1
    , shockSpeed = \_ _ -> 0
    , linearity = LDG
    }

rarefaction2 pt a =
    let v' = v pt
    in
    [u pt - a / (2 * v'), v']

shockSpeed2 ptL ptR = v ptL * (1 - u ptL - u ptR)

rSpeed2 pt = v pt * (1 - 2 * u pt)

field2 = CharField
    { λ = rSpeed2
    , r = \pt -> [-1 / (2 * v pt), 0]
    , rarefactionCurve = rarefaction2
    , shockCurve = rarefaction2
    , shockSpeed = shockSpeed2
    , linearity = GNL
    }

uWave :: Vector 2 -> Vector 2 -> Wave 2
uWave pL pR
  | u pL < u pR = shockWave field2 2 pL pR
  | otherwise = rarefactionWave field2 2 pL pR

vWave :: Vector 2 -> Vector 2 -> Wave 2
vWave _ _ = SWave Shock {speed = 0, sFamily = 1}

inDomain :: (Double, Double) -> Bool
inDomain (u, v) =  u > 0 && u < 1 && v > 0

solveRiemann' :: Vector 2 -> Vector 2 -> WaveFan 2
solveRiemann' pL pR
  | not (inDomain (uL, vL) && inDomain (uR, vR)) =
    error $ "TveitoWinther1995_3 solveRiemann: out of domain " ++
        show ((uL, vL), (uR, vR))
  | pL == pR = Fan [] pR
  | inv == 0 = Fan [(pL, vWave pL pR)] pR
  | vL == vR = Fan [(pL, uWave pL pR)] pR
  | uL <= 0.5 = solveRiemannLeft
  | otherwise = solveRiemannRight
  where
    (uL, vL) = (u pL, v pL)
    (uR, vR) = (u pR, v pR)

    solveRiemannLeft
      | uR >= 0.5 && inv < 0 = region2Left
      | uR < 0.5 && vR < 4 * uL * vL * (1 - uL) = region3Left
      | otherwise = region1Left

    region1Left =
        let uM = 0.5 * (1 - sqrt (1 + 4*uL*vL*(uL-1) / vR))
            pM = [uM, vR]
        in
        Fan [(pL, vWave pL pM), (pM, uWave pM pR)] pR

    region2Left =
        let uM = 0.5 * (1 + sqrt (1 + 4*uR*vR*(uR-1) / vL))
            pM = [uM, vL]
        in
        Fan [(pL, uWave pL pM), (pM, vWave pM pR)] pR

    region3Left =
        let uM1 = 0.5 * (1 + sqrt (1 - vR / vL))
            pM1 = [uM1, vL]
            pM2 = [0.5, vR]
        in
        Fan [(pL, uWave pL pM1), (pM1, vWave pM1 pM2), (pM2, uWave pM2 pR)] pR

    solveRiemannRight
      | uR >= 0.5 && inv2 <= 0 = region2Left -- same on the right
      | vR <= vL && uR <= 0.5 = region3Right
      | otherwise = region1Right

    region3Right =
        let uM1 = 0.5 * (1 + sqrt (1 - vR / vL))
            pM1 = [uM1, vL]
            pM2 = [0.5, vR]
        in
        Fan [(pL, uWave pL pM1), (pM1, vWave pM1 pM2), (pM2, uWave pM2 pR)] pR

    region1Right =
        let pM1 = [0.5, vL]
            uM2 = 0.5 * (1 - sqrt (1 - vL / vR))
            pM2 = [uM2, vR]
        in
        Fan [(pL, uWave pL pM1), (pM1, vWave pM1 pM2), (pM2, uWave pM2 pR)] pR

    -- inv == 0 along the v rarefaction through pL
    inv = vR * uR * (1 - uR) - vL * uL * (1 - uL)
    -- inv2 == 0 along the v rarefaction curve through (0.5, vL)
    inv2 = vR * uR * (1 - uR) - vL * 0.25

system :: System 2
system = System
    { flux = \p -> [(v p) * (u p) * (1 - u p), 0]
    , dFlux = \p ->
        let u' = u p
        in
        [[(v p) * (1 - 2 * u'), u' * (1 - u')], [0, 0]]
    , fields = [field1, field2]
    , solveRiemann = solveRiemann'
    }

