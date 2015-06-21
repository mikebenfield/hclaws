
module Math.ConservationLaws.Examples.ShallowWater (
    system,
    solution1,
) where

import Data.Maybe (fromJust)

import Data.Matrix ((!), fromLists)

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
        { λ = \m -> (q m) / (h m) - sqrt (h m)
        , r = \m -> (2/3) *. col [sqrt (h m), (q m) / sqrt (h m) + (h m)]
        , rarefactionCurve = rarefaction2
        , shockCurve = shock2
        , shockSpeed = speed2
        , linearity = GNL
        }

system :: System
system =
    System
        { n = 2
        , flux = \m -> col [q m, (q m)**2 / (h m) + (h m)^2/2]
        , dFlux = \m -> fromLists [[0, 1], [(h m) - (v m)^2, 2 * (v m)]]
        , fields = [field1, field2]
        , strengths = error "XXX"
        }

solution1 =
    Waves pt1 (Shock speed $ Just 1) $
    Last pt2
  where
    pt1 = col [1,1]
    pt2 = shock1 pt1 (-1)
    speed = speed1 pt1 pt2

