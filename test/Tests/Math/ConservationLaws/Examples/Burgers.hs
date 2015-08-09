{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.ConservationLaws.Examples.Burgers (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras
import Math.LinearAlgebra

import qualified Math.ConservationLaws as CL
import qualified Math.ConservationLaws.Examples.Burgers as B

import Math.Curves
import qualified Math.Integration as I

tests :: TestTree
tests = testGroup "Math.ConservationLaws.Examples.Burgers" [properties, unitTests]

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

testWaveFan :: CL.WaveFan 1 -> String -> TestTree
testWaveFan wf name = do
    HU.testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        CL.integrateFanOnCurve cc B.system wf @?~ [0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ testWaveFan B.solution1 "manual solution 1"
    , testWaveFan B.solution2 "manual solution 2"
    , testWaveFan B.solution3 "manual solution 3"
    , testWaveFan B.solution4 "computed solution 3"

    , testWaveFan (CL.solveRiemann B.system [-1] [1]) "Example 1"
    , testWaveFan (CL.solveRiemann B.system [13] [0]) "Example 2"
    , testWaveFan B.solution5 "computed solution 4"
    ]
