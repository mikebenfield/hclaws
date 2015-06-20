
module Tests.Math.ConservationLaws (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

import qualified Math.ConservationLaws as CL

tests :: TestTree
tests = testGroup "ConservationLaws" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [
    ]
