
module Tests.Math.Integration (
    tests
) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import qualified Math.Integration as I

import Math.LinearAlgebra

main = defaultMain tests

epsilon :: Double
epsilon = 0.1

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

tests :: TestTree
tests = testGroup "Math.Integration" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

form1 m = row [1, 1]
form2 m = row [I.t m, I.x m]
(segment1, segment1') = I.lineSegment_ (col [0, 0]) (col [2, 3])
(circle1, circle1') = I.circle_ (col [1, 1]) 1
(box1, box1') = I.box_ (col [0, 0]) 1 3

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ HU.testCase "simpson 1" $
          I.simpson (\x -> col [x^2]) 0 1 @?~ col [1/3]
    , HU.testCase "simpson 2" $
          I.simpson (\x -> col [sin x]) 0 pi @?~ col [2]
    , HU.testCase "adaptiveSimpson 1" $
          I.adaptiveSimpson 0.01 (\x -> col [sin x]) 0 pi @?~ col [2]
    , HU.testCase "adaptiveSimpson 2" $
          I.adaptiveSimpson 0.01 (\x -> col [x, x^2]) 0 1 @?~ col [1/2, 1/3]
    , HU.testCase "adaptiveSimpsonLineIntegral 1" $
          I.adaptiveSimpsonLineIntegral 
              0.01 segment1 segment1' form1 0 1 @?~ col [5]
    , HU.testCase "adaptiveSimpsonLineIntegral 2" $
          I.adaptiveSimpsonLineIntegral 
              0.01 segment1 segment1' form2 0 1 @?~ col [6]
    , HU.testCase "adaptiveSimpsonLineIntegral 3" $
          I.adaptiveSimpsonLineIntegral 
              0.01 circle1 circle1' form1 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 4" $
          I.adaptiveSimpsonLineIntegral 
              0.01 circle1 circle1' form2 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 5" $
          I.adaptiveSimpsonLineIntegral 
              0.0001 box1 box1' form1 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 6" $
          I.adaptiveSimpsonLineIntegral 
              0.0001 box1 box1' form2 0 1 @?~ col [0]
    ]

