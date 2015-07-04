
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
tests = testGroup "Math.ConservationLaws.Examples.Burgers" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

circle1 = C.Circle (col [0,0]) 1
box1 = C.Box (col [0,0]) 1 1

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
  [ HU.testCase "circle1 0" $
    C.at circle1 0 @?~ col [1, 0]
  , HU.testCase "circle1 0.25" $
    C.at circle1 0.25 @?~ col [0, 1]
  , HU.testCase "circle1 0.5" $
    C.at circle1 0.5 @?~ col [-1,0]
  , HU.testCase "circle1 0.75" $
    C.at circle1 0.75 @?~ col [0,-1]
  , HU.testCase "circle1 1" $
    C.at circle1 1 @?~ col [1,0]

  , HU.testCase "circle1' 0" $
    C.d circle1 0 @?~ col [0, 2*pi]
  , HU.testCase "circle1' 0.25" $
    C.d circle1 0.25 @?~ col [-2*pi, 0]
  , HU.testCase "circle1' 0.5" $
    C.d circle1 0.5 @?~ col [0,-2*pi]
  , HU.testCase "circle1' 0.75" $
    C.d circle1 0.75 @?~ col [2*pi,0]
  , HU.testCase "circle1' 1" $
    C.d circle1 1 @?~ col [0,2*pi]

  , HU.testCase "box1 0" $
    C.at box1 0 @?~ col [0, 0]
  , HU.testCase "box1 0.25" $
    C.at box1 0.25 @?~ col [1, 0]
  , HU.testCase "box1 0.5" $
    C.at box1 0.5 @?~ col [1,1]
  , HU.testCase "box1 0.75" $
    C.at box1 0.75 @?~ col [0,1]
  , HU.testCase "box1 1" $
    C.at box1 1 @?~ col [0,0]

  , HU.testCase "box1' 0" $
    C.d box1 0.1 @?~ col [4,0]
  , HU.testCase "box1' 0.25" $
    C.d box1 0.26 @?~ col [0, 4]
  , HU.testCase "box1' 0.6" $
    C.d box1 0.5 @?~ col [-4,0]
  , HU.testCase "box1' 0.76" $
    C.d box1 0.75 @?~ col [0,-4]
  ]

