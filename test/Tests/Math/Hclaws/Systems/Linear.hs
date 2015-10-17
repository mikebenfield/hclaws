{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Hclaws.Systems.Linear (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import Test.HUnitExtras

import Math.Hclaws.ConservationLaws
import Math.Hclaws.Curves
import Math.Hclaws.Integration
import Math.Hclaws.LinearAlgebra

import Math.Hclaws.Systems.Linear as L

tests :: TestTree
tests = testGroup "Math.ConservationLaws.Examples.Linear" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

testCurves :: [Curve]
testCurves =
    [ box (point (-5) 1) 20 20
    , box (point (-10) 2) 50 3
    , box (point (-30) 3) 100 20
    , circle (point 0 5) 4
    , circle (point (-1) 2) 1
    , circle (point 1 2) 1
    ]

testWaveFan :: WaveFan 3 -> String -> TestTree
testWaveFan wf name =
    testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        integrateFanOnCurve cc system wf @?~ [0, 0, 0]

solution1 = solveRiemann L.system [1, 2, 3] [2, 3, 4]

solution2 = solveRiemann L.system [17, 24, 29] [-100, -1, 3124]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testWaveFan solution1 "solution 1"
    , testWaveFan solution2 "solution 2"
    ]
