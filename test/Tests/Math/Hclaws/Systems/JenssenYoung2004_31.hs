{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Hclaws.Systems.JenssenYoung2004_31 (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras

import qualified Math.Hclaws.ConservationLaws as CL
import qualified Math.Hclaws.Systems.JenssenYoung2004_31 as JY

import Math.Hclaws.LinearAlgebra
import Math.Hclaws.Curves
import qualified Math.Hclaws.Integration as I

tests :: TestTree
tests =
    testGroup "Math.ConservationLaws.Examples.JenssenYoung2004_31"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

lambda = 1

testCurves :: [Curve]
testCurves =
    [ box (point (-5) 1) 20 20
    , box (point (-10) 2) 50 3
    , box (point (-30) 3) 100 20
    , circle (point 0 5) 4
    , circle (point (-1) 2) 1
    , circle (point 1 2) 1
    ]

testWaveFan :: CL.WaveFan 3 -> String -> TestTree
testWaveFan wf name =
    HU.testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        CL.integrateFanOnCurve cc (JY.system 1) wf @?~ [0,0,0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [
    ]
