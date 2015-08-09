
module Tests.Math.Curves (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras

import Math.LinearAlgebra

import qualified Math.Curves as C

import qualified Math.Integration as I

tests :: TestTree
tests = testGroup "Math.Curves" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

circle1 = C.circle (point 0 0) 1
box1 = C.box (point 0 0) 1 1
box2 = C.box (point (-5) 1) 20 20

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
  [ HU.testCase "circle1 0" $
    C.curve circle1 0 @?~ point 1 0
  , HU.testCase "circle1 0.25" $
    C.curve circle1 0.25 @?~ point 0 1
  , HU.testCase "circle1 0.5" $
    C.curve circle1 0.5 @?~ point (-1) 0
  , HU.testCase "circle1 0.75" $
    C.curve circle1 0.75 @?~ point 0 (-1)
  , HU.testCase "circle1 1" $
    C.curve circle1 1 @?~ point 1 0

  , HU.testCase "circle1' 0" $
    C.tangent circle1 0 @?~ point 0 (2*pi)
  , HU.testCase "circle1' 0.25" $
    C.tangent circle1 0.25 @?~ point (-2*pi) 0
  , HU.testCase "circle1' 0.5" $
    C.tangent circle1 0.5 @?~ point 0 (-2*pi)
  , HU.testCase "circle1' 0.75" $
    C.tangent circle1 0.75 @?~ point (2*pi) 0
  , HU.testCase "circle1' 1" $
    C.tangent circle1 1 @?~ point 0 (2*pi)

  , HU.testCase "box1 0" $
    C.curve box1 0 @?~ point 0 0
  , HU.testCase "box1 0.25" $
    C.curve box1 0.25 @?~ point 1 0
  , HU.testCase "box1 0.5" $
    C.curve box1 0.5 @?~ point 1 1
  , HU.testCase "box1 0.75" $
    C.curve box1 0.75 @?~ point 0 1
  , HU.testCase "box1 1" $
    C.curve box1 1 @?~ point 0 0

  , HU.testCase "box1' 0" $
    C.tangent box1 0.1 @?~ point 4 0
  , HU.testCase "box1' 0.25" $
    C.tangent box1 0.26 @?~ point 0 4
  , HU.testCase "box1' 0.6" $
    C.tangent box1 0.5 @?~ point (-4) 0
  , HU.testCase "box1' 0.76" $
    C.tangent box1 0.75 @?~ point 0 (-4)

  , HU.testCase "box2 0" $
    C.curve box2 0 @?~ point (-5) 1
  , HU.testCase "box2 0.25" $
    C.curve box2 0.25 @?~ point (15) 1
  , HU.testCase "box2 0.5" $
    C.curve box2 0.5 @?~ point (15) 21
  , HU.testCase "box2 0.75" $
    C.curve box2 0.75 @?~ point (-5) 21
  , HU.testCase "box2 1" $
    C.curve box2 1 @?~ point (-5) 1

  , HU.testCase "box2' 0.1" $
    C.tangent box2 0.1 @?~ point 80 0
  , HU.testCase "box2' 0.2" $
    C.tangent box2 0.2 @?~ point 80 0
  , HU.testCase "box2' 0.4" $
    C.tangent box2 0.4 @?~ point 0 80
  , HU.testCase "box2' 0.7" $
    C.tangent box2 0.7 @?~ point (-80) 0
  , HU.testCase "box2' 0.9" $
    C.tangent box2 0.9 @?~ point 0 (-80)

  ]
