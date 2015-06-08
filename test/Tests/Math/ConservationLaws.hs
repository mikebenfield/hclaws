
module Tests.Math.ConservationLaws (
  tests
) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
import Math.ConservationLaws as CL

main = defaultMain tests

tests :: TestTree
tests = testGroup "ConservationLaws" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
  [ QC.testProperty "bogus property" $
    ((\x -> CL.aFunction == 5):: Int -> Bool)
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" []
