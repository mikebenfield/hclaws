
module Test.HUnitExtras (
    (@?~),
    (@~?),
    (@?~~),
    (@~~?)
) where

import qualified Test.Tasty.HUnit as HU

import Math.LinearAlgebra

-- since the goal is not to test the accuracy of the numerical algorithms,
-- maybe a large epsilon is reasonable?
epsilon :: Double
epsilon = 0.01

infix 1 @?~~
(@?~~) :: Point -> Point -> HU.Assertion
l@Point {x = x', t = t'} @?~~ r@Point {x = x'', t = t''}
  | abs (x' - x'') < epsilon && abs (t' - t'') < epsilon = HU.assertString ""
  | otherwise =
        HU.assertString $
            "expected (approximately):\n" ++ (show r) ++ "\n"
         ++ " but got:\n" ++ (show l)

infix 1 @~~?
(@~~?) :: Point -> Point -> HU.Assertion
(@~~?) = flip (@?~~)

infix 1 @?~
(@?~) :: Mat -> Mat -> HU.Assertion
l @?~ r
  | normP (1/0) (l-r) < epsilon = HU.assertString ""
  | otherwise =
        HU.assertString $
            "expected (approximately):\n" ++ (show r) ++ "\n"
         ++ " but got:\n" ++ (show l)

infix 1 @~?
(@~?) :: Mat -> Mat -> HU.Assertion
(@~?) = flip (@?~)
