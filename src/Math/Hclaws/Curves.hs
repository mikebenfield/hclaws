
module Math.Hclaws.Curves (
    Curve(..),
    box,
    circle
) where

import Math.Hclaws.LinearAlgebra

data Curve = Curve
    { curve :: Double -> Point
    , tangent :: Double -> Point
    }

box :: Point -> Double -> Double -> Curve
box Point{..} width height = Curve
    { curve = \s ->
        select
            (point (x + scale s * width) t)
            (point (x + width) (t + scale s * height))
            (point (x + (1 - scale s) * width) (t + height))
            (point x (t + (1 - scale s) * height))
            s
    , tangent = \s ->
        select
            (point (4 * width) 0)
            (point 0 (4 * height))
            (point (-4 * width) 0)
            (point 0 (-4 * height))
            s
    }
  where
    fracPart s = s - (fromIntegral (floor s :: Int))
    -- The next line actually matters. Having a jump discontinuity on the last
    -- point will screw with the adapted simpson's rule.
    scale 1 = 1
    scale s = fracPart (4 * fracPart s)
    select pt1 pt2 pt3 pt4 s
      | s < 0.25 = pt1
      | s < 0.5 = pt2
      | s < 0.75 = pt3
      | otherwise = pt4

circle :: Point -> Double -> Curve
circle Point{..} radius = Curve
    { curve = \s ->
        point (x + radius * cos (2*pi*s)) (t + radius * sin (2*pi*s))
    , tangent = \s ->
        point (-2*pi*radius * sin (2*pi*s)) (2*pi*radius * cos (2*pi*s))
    }
