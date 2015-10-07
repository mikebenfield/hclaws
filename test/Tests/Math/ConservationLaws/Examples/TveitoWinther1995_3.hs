{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.ConservationLaws.Examples.TveitoWinther1995_3 (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Test.HUnitExtras
import Math.LinearAlgebra

import Math.Fan
import Math.ConservationLaws
import qualified Math.ConservationLaws.Examples.TveitoWinther1995_3 as TW

import Math.Curves
import qualified Math.Integration as I

tests :: TestTree
tests =
    testGroup "Math.ConservationLaws.Examples.JenssenYoung2004_31"
        [properties, solutionTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = $(testGroupGenerator)

testCurves :: [Curve]
testCurves =
    [ box (point (-5) 1) 20 20
    , box (point (-10) 2) 50 3
    , box (point (-30) 3) 100 20
    , circle (point 0 5) 4
    , circle (point (-1) 2) 1
    , circle (point 1 2) 1
    ]

testWaveFan :: WaveFan 2 -> String -> TestTree
testWaveFan wf name =
    testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        integrateFanOnCurve cc TW.system wf @?~ [0,0]

solution1 = solveRiemann TW.system [0.3, 1] [0.2, 0.05]

solutionTests :: TestTree
solutionTests = testGroup "UnitTests" $
    [ testWaveFan
        (solveRiemann TW.system [0.3, 1] [0.2, 1.2])
        "left region 1"
    , testWaveFan
        (solveRiemann TW.system [0.3, 1] [0.9, 0.5])
        "left region 2"
    , testWaveFan
        (solveRiemann TW.system [0.3, 1] [0.2, 0.05])
        "left region 3"
    , testWaveFan
        (solveRiemann TW.system [0.7, 1] [0.8, 0.5])
        "right region 2"
    , testWaveFan
        (solveRiemann TW.system [0.7, 1] [0.1, 1.2])
        "right region 1"
    , testWaveFan
        (solveRiemann TW.system [0.7, 1] [0.1, 0.1])
        "right region 3"
    , testWaveFan
        (solveRiemann TW.system [0.5, 0.5] [0.5, 0.5])
        "constant"
    ]
