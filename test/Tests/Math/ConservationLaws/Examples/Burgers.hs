
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

import qualified Math.Integration as I

tests :: TestTree
tests = testGroup "Math.ConservationLaws.Examples.Burgers" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

c1 = I.lineSegment_ (col [-0.5, 0.5]) (col [1.5, 0.5])
c2 = I.lineSegment_ (col [1.5, 0.5]) (col [1.5, 2.5])
c3 = I.lineSegment_ (col [1.5, 2.5]) (col [-0.5, 2.5])
c4 = I.lineSegment_ (col [-0.5, 2.5]) (col [-0.5, 0.5])
c5 = (I.glueCurves [fst c1, fst c2], I.glueCurves [snd c1, snd c2])

testCurves =
    [ I.box_ (col [-5, 1]) 20 20
    , I.box_ (col [-10, 2]) 50 3
    , I.box_ (col [-30, 3]) 100 20
    , I.circle_ (col [0, 5]) 4
    , I.circle_ (col [-1, 2]) 1
    , I.circle_ (col [1, 2]) 1
    ]

testWaveFan :: CL.WaveFan -> String -> TestTree
testWaveFan wf name = do
    HU.testCase name $ mapM_ intOnCurve testCurves
  where
    intOnCurve cc =
        CL.integrateFanOnCurve cc B.system wf @?~ col [0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ testWaveFan B.solution1 "manual solution 1"
    , testWaveFan B.solution2 "manual solution 2"
    , testWaveFan B.solution3 "manual solution 3"
    , testWaveFan B.solution4 "computed solution 3"

    , testWaveFan (CL.solveRiemann B.system (col [-1]) (col [1])) "Example 1"
    , testWaveFan (CL.solveRiemann B.system (col [13]) (col [0])) "Example 2"
    , testWaveFan B.solution5 "computed solution 4"
    ]

