
module Tests.Math.ConservationLaws.Examples.TwoComponentChromatography (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras
import Math.LinearAlgebra

import qualified Math.ConservationLaws as CL
import qualified Math.ConservationLaws.Examples.TwoComponentChromatography as TCC

import qualified Math.Integration as I

tests :: TestTree
tests =
    testGroup "Math.ConservationLaws.Examples.TwoComponentChromatography"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

testCurves =
    [ I.box_ (col [-5, 1]) 20 20
    , I.box_ (col [-10, 2]) 50 3
    , I.box_ (col [-30, 3]) 100 20
    , I.circle_ (col [0, 5]) 4
    , I.circle_ (col [-1, 2]) 1
    , I.circle_ (col [1, 2]) 1
    ]

testWaveFan :: CL.WaveFan -> String -> TestTree
testWaveFan wf name =
    HU.testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        CL.integrateFanOnCurve cc (TCC.system 1) wf @?~ col [0,0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ testWaveFan TCC.solution1 "computed solution 1"
    , testWaveFan TCC.solution2 "computed solution 2"
    , testWaveFan TCC.solution3 "computed solution 3"
    , testWaveFan TCC.solution4 "computed solution 4"
    , testWaveFan TCC.solution5 "computed solution 5"
    ]

