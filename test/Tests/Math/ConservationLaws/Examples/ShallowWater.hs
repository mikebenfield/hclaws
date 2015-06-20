
module Tests.Math.ConservationLaws.Examples.ShallowWater (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras
import Math.LinearAlgebra

import qualified Math.ConservationLaws as CL
import qualified Math.ConservationLaws.Examples.ShallowWater as SW

import qualified Math.Integration as I

tests :: TestTree
tests =
    testGroup "Math.ConservationLaws.Examples.ShallowWater"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

testCurves =
    [ I.box_ (col [0, 1]) 1 2
    , I.box_ (col [0, 0]) 2 2
    , I.box_ (col [-5, 1]) 20 2
    , I.circle_ (col [1, 1]) 1
    , I.circle_ (col [0, 1]) 0.5
    , I.circle_ (col [5,5]) 3
    ]

curvesAndWaveFanCase msg (c, c') s wf =
    HU.testCase msg $
        CL.checkSolnOnCurve c c' s (CL.evalWaveFanAtPoint wf) @?~ col [0,0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    map (\(c_, i) -> curvesAndWaveFanCase (show i) c_ SW.system SW.solution1) $
        zip testCurves [1..]

