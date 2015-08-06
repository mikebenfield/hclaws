
module Math.ConservationLaws.Examples.TwoComponentChromatography (
system,
solution1, solution2, solution3, solution4, solution5
) where

import Data.Matrix ((!), fromLists)

import Math.LinearAlgebra
import Math.ConservationLaws

u m = m ! (1, 1)
v m = m ! (2, 1)

f k m a = 1 + (sqrt (k/(a+k)) - 1) / (u m + v m)

lambda1 k m = k / (1 + u m + v m)^(2::Int)

rarefaction1 k m a =
    f k m a *. m

shock1 = rarefaction1

speed1 k ul ur =
    k * v ul / ( (1 + u ul + v ul) * (u ul * v ur + v ul * (1 + v ur) ) )

r1 k m =
    col [factor * u m, factor * v m]
  where
    factor = - (1 + u m + v m)^(2::Int) / (2*k*(u m + v m))

field1 :: Double -> CharField
field1 k =
    CharField
        { λ = lambda1 k
        , r = r1 k
        , rarefactionCurve = rarefaction1 k
        , shockCurve = shock1 k
        , shockSpeed = speed1 k
        , linearity = GNL
        }

lambda2 k m = k / (1 + u m + v m)

rarefaction2 _ m b = col [u m - b, v m + b]

shock2 = rarefaction2

speed2 k ul _ = lambda2 k ul

field2 :: Double -> CharField
field2 k =
    CharField
        { λ = lambda2 k
        , r = \_ -> col [-1, 1]
        , rarefactionCurve = rarefaction2 k
        , shockCurve = shock2 k
        , shockSpeed = speed2 k
        , linearity = LDG
        }

system :: Double -> System
system k =
    System
        { n = 2
        , flux = \m -> col [k*(u m)/(1 + u m + v m), k*(v m)/(1 + u m + v m)]
        , dFlux = \m ->
            k / (1 + u m + v m)^(2::Int) *.
                fromLists [[1 + v m, - u m], [-v m, 1 + u m]]
        , fields = fs
        , solveRiemann = \mL mR ->
            let b = (-u mR * v mL + v mR * u mL) / (u mL + v mL)
                fm1 = (u mR + v mR) / (u mL + v mL)
                a = -k + k / (1 + fm1*(u mL + v mL))^(2::Int)
             in
             strengthsToFan mL fs [a, b]
        }
      where
        fs = [field1 k, field2 k]

solution1 = solveRiemann (system 1) (col [1,1]) (col [2,2])
solution2 = solveRiemann (system 1) (col [3,4]) (col [8,1])
solution3 = solveRiemann (system 1) (col [1,1]) (col [1,2])
solution4 = solveRiemann (system 1) (col [1,1]) (col [0.5,0.5])
solution5 = solveRiemann (system 1) (col [1,1]) (col [2,0.5])
