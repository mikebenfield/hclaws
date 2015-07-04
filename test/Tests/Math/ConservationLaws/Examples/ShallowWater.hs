
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

import qualified Math.Curves as C
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
    [ C.Box (col [-5, 1]) 20 20
    , C.Box (col [-10, 2]) 50 3
    , C.Box (col [-30, 3]) 100 20
    , C.Circle (col [0, 5]) 4
    , C.Circle (col [-1, 2]) 1
    , C.Circle (col [1, 2]) 1
    ]

testWaveFan :: CL.WaveFan -> String -> TestTree
testWaveFan wf name =
    HU.testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        CL.integrateFanOnCurve cc SW.system wf @?~ col [0,0]

solution2 = CL.solveRiemann SW.system (col [1,1]) (col [2,2])
unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ testWaveFan SW.solution1 "manual solution 1"
    , testWaveFan SW.solution2 "computed solution 2"
    , testWaveFan SW.solution3 "computed solution 3"
    , testWaveFan SW.solution4 "computed solution 4"
    , testWaveFan SW.solution5 "computed solution 5"
    ]

