{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Hclaws.Systems.TwoComponentChromatography (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.Curves

import qualified Math.Hclaws.Systems.TwoComponentChromatography as TCC

tests :: TestTree
tests =
    testGroup "Math.Hclaws.Systems.TwoComponentChromatography"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ waveFanTestGroup TCC.solution1 (TCC.system 1) "computed solution 1"
    , waveFanTestGroup TCC.solution2 (TCC.system 1) "computed solution 2"
    , waveFanTestGroup TCC.solution3 (TCC.system 1) "computed solution 3"
    , waveFanTestGroup TCC.solution4 (TCC.system 1) "computed solution 4"
    , waveFanTestGroup TCC.solution5 (TCC.system 1) "computed solution 5"
    ]
