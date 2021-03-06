{-# LANGUAGE DataKinds #-}

module Math.Hclaws.Systems.ShallowWater (
    system,
    solution1,
    solution2, solution3, solution4, solution5,
) where

import Data.Proxy

import Math.FTensor.Algebra
import Math.FTensor.General as F

import Math.Hclaws.Fan
import Math.Hclaws.ConservationLaws

h :: F.TensorBoxed '[2] Double -> Double
h t = F.pIndex t (Proxy::Proxy '[0])
q :: F.TensorBoxed '[2] Double -> Double
q t = F.pIndex t (Proxy::Proxy '[1])
v m = (q m) / (h m)

rarefaction1 m a =
    [hn, hn * (a*2/3 + (q m) / (h m))]
  where
    hn = (sqrt (h m) - a/3)**2

shock1 m a =
    [hn, (q m) * hn / h0 - hn * (hn-h0) * sqrt (1/hn + 1/h0) / sqrt 2]
  where
    h0 = h m
    hn = h0 - a

speed1 ul ur =
    (v ul) - hr * sqrt (1/hr + 1/(h ul)) / sqrt 2
  where
    hr = h ur

field1 :: CharField 2
field1 =
    CharField
        { λ = \m -> (q m) / (h m) - sqrt (h m)
        , r = \m -> (-2/3) *: [sqrt (h m), (q m) / sqrt (h m) - (h m)]
        , rarefactionCurve = rarefaction1
        , shockCurve = shock1
        , shockSpeed = speed1
        , linearity = GNL
        }

rarefaction2 m a =
    [hn, hn * (a*2/3 + (q m) / (h m))]
  where
    hn = (sqrt (h m) + a/3)**2

shock2 m a =
    [hn, (q m) * hn / h0 + hn * (hn-h0) * sqrt (1/hn + 1/h0) / sqrt 2]
  where
    h0 = h m
    hn = h0 + a

speed2 ul ur =
    (v ul) + hr * sqrt (1/hr + 1/(h ul)) / sqrt 2
  where
    hr = h ur

field2 :: CharField 2
field2 =
    CharField
        { λ = \m -> (q m) / (h m) + sqrt (h m)
        , r = \m -> (2/3) *: [sqrt (h m), (q m) / sqrt (h m) + (h m)]
        , rarefactionCurve = rarefaction2
        , shockCurve = shock2
        , shockSpeed = speed2
        , linearity = GNL
        }

newton1 f f' x0 =
    x0 - (f x0) / (f' x0)

newtonN :: (Double -> Double) -> (Double -> Double) -> Double -> Int -> Double
newtonN _ _ x0 0 = x0
newtonN f f' x0 n = newtonN f f' (newton1 f f' x0) (n-1)

solveRiemann' :: Vector 2 -> Vector 2 -> WaveFan 2
solveRiemann' uL uR
  | hL <= 0 || hR <= 0 || r2 uR >= r1 uL = -- region V or out of domain
    error "ShallowWater solveRiemann: out of domain"
  | uL == uR = Fan [] uL
  | r1 uR == r1 uL && hR < hL = -- along R1
    Fan [(uL, rarefactionWave field1 1 uL uR)] uR
  | r2 uR == r2 uL && hR > hL = -- along R2
    Fan [(uL, rarefactionWave field2 2 uL uR)] uR
  | s1 uR == s1 uL && hR > hL = -- along S1
    Fan [(uL, shockWave field1 1 uL uR)] uR
  | s2 uR == s2 uL && hR < hL = -- along S2
    Fan [(uL, shockWave field2 2 uL uR)] uR
  | r2 uR < r2 uL && s1 uR > s1 uL = -- region I, S1.R2
    let hM = newtonN (f hR hL vR vL) (f' hR hL) ((hL+hR)/2) 50
        qM = hM * (vL - (1 / sqrt 2) * (hM-hL) * c hM hL)
        uM = [hM, qM]
    in
    waveFan uM shockWave rarefactionWave
  | r1 uR > r1 uL && r2 uR > r2 uL = -- region II, R1.R2
    let
        hM = ((2 * (sqrt hR + sqrt hL) - vR + vL) / 4)^(2::Int)
        qM = (r1 uL) * hM - 2 * hM**1.5
        uM = [hM, qM]
    in
    waveFan uM rarefactionWave rarefactionWave
  | r1 uR < r1 uL && s2 uR > s2 uL = -- region III, R1.S2
    let
        -- the eqn that determines hM in region III is the same
        -- as that for region I, but with the roles of uL, uR
        -- reversed
        hM = newtonN (f hL hR vL vR) (f' hL hR) ((hL+hR)/2) 50
        qM = (r1 uL) * hM - 2 * hM**1.5
        uM = [hM, qM]
    in
    waveFan uM rarefactionWave shockWave
  | s1 uR < s1 uL && s2 uR < s2 uL = -- region IV, S1.S2
    let hM = newtonN g g' ((hL+hR)/2) 50
        qM = hM * (vL - (hM-hL) * c hM hL / sqrt 2)
        uM = [hM, qM]
    in
    waveFan uM shockWave shockWave
  | otherwise =
      error "ShallowWater solveRiemann: can't happen"
  where
    waveFan
        :: Vector 2 -> (CharField 2 -> Int -> Vector 2 -> Vector 2 -> Wave 2)
        -> (CharField 2 -> Int -> Vector 2 -> Vector 2 -> Wave 2)
        -> WaveFan 2
    waveFan uM waveA waveB =
        Fan [(uL, waveA field1 1 uL uM), (uM, waveB field2 2 uM uR)] uR
    hL = h uL
    qL = q uL
    hR = h uR
    vR = v uR
    vL = v uL
    c hr hm = sqrt (1/hr + 1/hm)
    d hr hm = (hr - hm) * c hr hm
    f hr hl vr vl hm =
        2 * sqrt 2 * (sqrt hr - sqrt hm) - (hm-hl)*sqrt(1/hm+1/hl) -
          sqrt 2 * (vr - vl)
    f' hr hl hm =
        negate (c hr hm) + (hm - hl) / (2*hm^(2::Int)*c hr hm) - sqrt (2 / hm)
    g hm = d hR hm + d hL hm - sqrt 2 * (vR - vL)
    g' hm =
        (hm-hL) / (2 * hm^(2::Int) * c hL hm) - c hL hm +
        (hm-hR) / (2 * hm^(2::Int) * c hR hm) - c hR hm
    r1 pt = (v pt) + 2 * sqrt (h pt)
    r2 pt = (v pt) - 2 * sqrt (h pt)
    s1 pt =
        (q pt) - qL * (h pt) / hL +
            (h pt) * (h pt - hL) * sqrt (1/(h pt) + 1/hL) / sqrt 2
    s2 pt =
        (q pt) - qL * (h pt) / hL -
            (h pt) * (h pt - hL) * sqrt (1/(h pt) + 1/hL) / sqrt 2

system :: System 2
system =
    System
        { flux = \m -> [q m, (q m)**2 / (h m) + (h m)^(2::Int)/2]
        , dFlux = \m -> [[0, 1], [(h m) - (v m)^(2::Int), 2 * (v m)]]
        , fields = [field1, field2]
        , solveRiemann = solveRiemann'
        }

solution1 :: WaveFan 2
solution1 =
    Fan [(pt1, SWave Shock {speed = speed', sFamily = 0})] pt2
  where
    pt1 = [1,1]
    pt2 = shock1 pt1 (-1)
    speed' = speed1 pt1 pt2

solution2 = solveRiemann system [1,1] [2,2]
solution3 = solveRiemann system [1,1] [1,2]
solution4 = solveRiemann system [1,1] [0.5,0.5]
solution5 = solveRiemann system [1,1] [1,0]
