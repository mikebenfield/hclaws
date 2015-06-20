
module Tests.Math.ConservationLaws (
    tests
) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

import qualified Math.ConservationLaws as CL

main = defaultMain tests

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
