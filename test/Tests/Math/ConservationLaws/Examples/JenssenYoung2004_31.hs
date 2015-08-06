
module Tests.Math.ConservationLaws.Examples.JenssenYoung2004_31 (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras
import Math.LinearAlgebra

import qualified Math.ConservationLaws as CL
import qualified Math.ConservationLaws.Examples.JenssenYoung2004_31 as JY

import qualified Math.Curves as C
import qualified Math.Integration as I

tests :: TestTree
tests =
    testGroup "Math.ConservationLaws.Examples.JenssenYoung2004_31"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

lambda = 1

testCurves =
    [ C.Box (col [-5, 1]) 20 20
    , C.Box (col [-10, 2]) 50 3
    , C.Box (col [-30, 3]) 100 20
    , C.Circle (col [0, 5]) 4
    , C.Circle (col [-1, 2]) 1
    , C.Circle (col [1, 2]) 1
    ]

testWaveFan :: CL.WaveFan -> String -> TestTree
testWaveFan wf name =
    HU.testCase name $ mapM_ intOnCurve [testCurves !! 1]
  where
    intOnCurve cc =
        CL.integrateFanOnCurve cc (JY.system 1) wf @?~ col [0,0,0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [
    ]
