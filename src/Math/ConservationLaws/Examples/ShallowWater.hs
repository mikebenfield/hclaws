
module Math.ConservationLaws.Examples.ShallowWater (
    system,
    solution1,
    solution2, solution3, solution4, solution5,
) where

import Data.Matrix ((!), fromLists)

import Math.Fan
import Math.LinearAlgebra
import Math.ConservationLaws

h m = m ! (1, 1)
q m = m ! (2, 1)
v m = (q m) / (h m)

rarefaction1 m a =
    col [hn, hn * (a*2/3 + (q m) / (h m))]
  where
    hn = (sqrt (h m) - a/3)**2

shock1 m a =
    col [hn, (q m) * hn / h0 - hn * (hn-h0) * sqrt (1/hn + 1/h0) / sqrt 2]
  where
    h0 = h m
    hn = h0 - a

speed1 ul ur =
    (v ul) - hr * sqrt (1/hr + 1/(h ul)) / sqrt 2
  where
    hr = h ur

field1 :: CharField
field1 =
    CharField
        { λ = \m -> (q m) / (h m) - sqrt (h m)
        , r = \m -> (-2/3) *. col [sqrt (h m), (q m) / sqrt (h m) - (h m)]
        , rarefactionCurve = rarefaction1
        , shockCurve = shock1
        , shockSpeed = speed1
        , linearity = GNL
        }

rarefaction2 m a =
    col [hn, hn * (a*2/3 + (q m) / (h m))]
  where
    hn = (sqrt (h m) + a/3)**2

shock2 m a =
    col [hn, (q m) * hn / h0 + hn * (hn-h0) * sqrt (1/hn + 1/h0) / sqrt 2]
  where
    h0 = h m
    hn = h0 + a

speed2 ul ur =
    (v ul) + hr * sqrt (1/hr + 1/(h ul)) / sqrt 2
  where
    hr = h ur

field2 :: CharField
field2 =
    CharField
        { λ = \m -> (q m) / (h m) + sqrt (h m)
        , r = \m -> (2/3) *. col [sqrt (h m), (q m) / sqrt (h m) + (h m)]
        , rarefactionCurve = rarefaction2
        , shockCurve = shock2
        , shockSpeed = speed2
        , linearity = GNL
        }

newton1 f f' x0 =
    x0 - (f x0) / (f' x0)

newtonN f f' x0 0 = x0
newtonN f f' x0 n = newtonN f f' (newton1 f f' x0) (n-1)

solveRiemann' :: Mat -> Mat -> WaveFan
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
        uM = col [hM, qM]
    in
    waveFan uM shockWave rarefactionWave
  | r1 uR > r1 uL && r2 uR > r2 uL = -- region II, R1.R2
    let
        hM = ((2 * (sqrt hR + sqrt hL) - vR + vL) / 4)^2
        qM = (r1 uL) * hM - 2 * hM**1.5
        uM = col [hM, qM]
    in
    waveFan uM rarefactionWave rarefactionWave
  | r1 uR < r1 uL && s2 uR > s2 uL = -- region III, R1.S2
    let
        -- the eqn that determines hM in region III is the same
        -- as that for region I, but with the roles of uL, uR
        -- reversed
        hM = newtonN (f hL hR vL vR) (f' hL hR) ((hL+hR)/2) 50
        qM = (r1 uL) * hM - 2 * hM**1.5
        uM = col [hM, qM]
    in
    waveFan uM rarefactionWave shockWave
  | s1 uR < s1 uL && s2 uR < s2 uL = -- region IV, S1.S2
    let hM = newtonN g g' ((hL+hR)/2) 50
        qM = hM * (vL - (hM-hL) * c hM hL / sqrt 2)
        uM = col [hM, qM]
    in
    waveFan uM shockWave shockWave
  | otherwise =
      error "ShallowWater solveRiemann: can't happen"
  where
    waveFan :: Mat -> (CharField -> Int -> Mat -> Mat -> Wave)
            -> (CharField -> Int -> Mat -> Mat -> Wave)
            -> WaveFan
    waveFan uM waveA waveB =
        Fan [(uL, waveA field1 1 uL uM), (uM, waveB field2 2 uM uR)] uR
    hL = h uL
    qL = q uL
    hR = h uR
    qR = q uR
    vR = v uR
    vL = v uL
    c hr hm = sqrt (1/hr + 1/hm)
    d hr hm = (hr - hm) * c hr hm
    f hr hl vr vl hm =
        2 * sqrt 2 * (sqrt hr - sqrt hm) - (hm-hl)*sqrt(1/hm+1/hl) -
          sqrt 2 * (vr - vl)
    f' hr hl hm =
        negate (c hr hm) + (hm - hl) / (2*hm^2*c hr hm) - sqrt (2 / hm)
    g hm = d hR hm + d hL hm - sqrt 2 * (vR - vL)
    g' hm =
        (hm-hL) / (2 * hm^2 * c hL hm) - c hL hm +
        (hm-hR) / (2 * hm^2 * c hR hm) - c hR hm
    r1 pt = (v pt) + 2 * sqrt (h pt)
    r2 pt = (v pt) - 2 * sqrt (h pt)
    s1 pt =
        (q pt) - qL * (h pt) / hL +
            (h pt) * (h pt - hL) * sqrt (1/(h pt) + 1/hL) / sqrt 2
    s2 pt =
        (q pt) - qL * (h pt) / hL -
            (h pt) * (h pt - hL) * sqrt (1/(h pt) + 1/hL) / sqrt 2

system :: System
system =
    System
        { n = 2
        , flux = \m -> col [q m, (q m)**2 / (h m) + (h m)^2/2]
        , dFlux = \m -> fromLists [[0, 1], [(h m) - (v m)^2, 2 * (v m)]]
        , fields = [field1, field2]
        , solveRiemann = solveRiemann'
        }

solution1 :: WaveFan
solution1 =
    Fan [(pt1, SWave Shock {speed = speed', sFamily = 0})] pt2
  where
    pt1 = col [1,1]
    pt2 = shock1 pt1 (-1)
    speed' = speed1 pt1 pt2

solution2 = solveRiemann system (col [1,1]) (col [2,2])
solution3 = solveRiemann system (col [1,1]) (col [1,2])
solution4 = solveRiemann system (col [1,1]) (col [0.5,0.5])
solution5 = solveRiemann system (col [1,1]) (col [1,0])

