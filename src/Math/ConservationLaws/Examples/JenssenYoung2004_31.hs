
module Math.ConservationLaws.Examples.JenssenYoung2004_31 (
    system,
    solution1,
    solution2,
    solution3,
) where

import Math.LinearAlgebra
import Math.ConservationLaws

import Data.Matrix ((!), fromLists)

u m = m ! (1, 1)
v m = m ! (2, 1)
w m = m ! (3, 1)

rarefaction1 m a = col [(u m) - a * cos (v m), (v m), (w m) + a * sin (v m)]
field1 lambda =
    CharField
        { λ = \_ -> -lambda
        , r = \m -> col [- cos (v m), 0, sin (v m)]
        , rarefactionCurve = rarefaction1
        , shockCurve = rarefaction1
        , shockSpeed = \_ _ -> -lambda
        , linearity = LDG
        }

-- XXX
-- This system is wrong because this field does not have the constant
-- eigenvector [0,1,0]. This needs to be fixed. 
rarefaction2 m a = col [(u m), (v m) + a, (w m)]
field2 =
    CharField
        { λ = \m -> (v m)
        , r = \_ -> col [0, 1, 0]
        , rarefactionCurve = rarefaction2
        , shockCurve = rarefaction2
        , shockSpeed = \mL mR -> ((v mL) + (v mR)) / 2
        , linearity = GNL
        }

rarefaction3 m a = col [(u m) + a * sin (v m), v m, (w m) + a * cos (v m)]
field3 lambda =
    CharField
        { λ = \_ -> lambda
        , r = \m -> col [sin (v m), 0, cos (v m)]
        , rarefactionCurve = rarefaction3
        , shockCurve = rarefaction3
        , shockSpeed = \_ _ -> lambda
        , linearity = LDG
        }

flux' lambda m =
    col [-lambda * c2v * (u m) + lambda * s2v * (w m),
         (v m)^2/2,
         lambda * s2v * (u m) + lambda * c2v * (w m)]
  where
    c2v = cos (2 * (v m))
    s2v = sin (2 * (v m))


dFlux' lambda m =
    fromLists
        [[-lambda * c2v, 2 * lambda * (wm * c2v + um * s2v), lambda * s2v],
         [0, vm, 0],
         [lambda * s2v, 2 * lambda * (um * c2v - wm * s2v), lambda * c2v]]
  where
    um = u m
    vm = v m
    wm = w m
    c2v = cos (2 * vm)
    s2v = sin (2 * vm)

solveRiemann' lambda mL mR
  | cdv == 0 = error "solveRiemann' : domain"
  | otherwise =
      strengthsToFan mL [field1 lambda, field2, field3 lambda] [a, diffV, c]
  where
    diffU = (u mR) - (u mL)
    diffV = (v mR) - (v mL)
    diffW = (w mR) - (w mL)
    cdv = cos diffV
    a = (-cos (v mR) * diffU + sin (v mR) * diffW) / cdv
    c = (sin (v mL) * diffU + cos (v mL) * diffW) / cdv

system :: Double -> System
system lambda =
    System
        { n = 3
        , flux = flux' lambda
        , dFlux = dFlux' lambda
        , fields = [field1 lambda, field2, field3 lambda]
        , solveRiemann = solveRiemann' lambda
        }

-- XXX solution2 and solution3 are not working!
solution1 lambda = solveRiemann' lambda (col [0,0,0]) (col [1,pi/4,1])
solution2 lambda = solveRiemann' lambda (col [0,0,0]) (col [1,pi/3,1])
solution3 lambda = solveRiemann' lambda (col [0,0,0]) (col [0,pi/4,1])

