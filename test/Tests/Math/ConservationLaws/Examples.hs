
module Tests.Math.ConservationLaws.Examples (
    tests
) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import qualified Math.ConservationLaws as CL
import qualified Math.ConservationLaws.Examples as E
import qualified Math.Integration as I

import Math.LinearAlgebra

main = defaultMain tests

epsilon :: Double
epsilon = 0.01

infix 1 @?~
(@?~) :: Mat -> Mat -> HU.Assertion
l @?~ r
  | normP (1/0) (l-r) < epsilon = HU.assertString ""
  | otherwise =
        HU.assertString $
            "expected: " ++ (show r) ++ " (approximately)\n" ++
                " but got: " ++ (show l)

infix 1 @~?
(@~?) :: Mat -> Mat -> HU.Assertion
(@~?) = flip (@?~)

tests :: TestTree
tests = testGroup "Math.ConservationLaws.Examples" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

testCurves = 
    [ I.box_ (col [0, 1]) 1 2
    , I.box_ (col [0, 0]) 2 2
    , I.circle_ (col [1, 1]) 1
    , I.circle_ (col [0, 1]) 1
    , I.circle_ (col [5,5]) 3
    ]

curvesAndWaveFanCase msg (c, c') s wf =
    HU.testCase msg $
      CL.checkSolnOnCurve c c' s (CL.evalWaveFanAtPoint wf) @?~ col [0]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    map (\(c_, i) -> curvesAndWaveFanCase (show i) c_ E.burgers E.burgersSolution1) $
        zip testCurves [1..]

