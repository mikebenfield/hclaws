{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists#-}

module Tests.Math.Hclaws.Systems.ShallowWater (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.Curves

import qualified Math.Hclaws.Systems.ShallowWater as SW

tests :: TestTree
tests =
    testGroup "Math.Hclaws.Systems.ShallowWater"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ waveFanTestGroup SW.solution1 SW.system "manual solution 1"
    , waveFanTestGroup SW.solution2 SW.system "computed solution 2"
    , waveFanTestGroup SW.solution3 SW.system "computed solution 3"
    , waveFanTestGroup SW.solution4 SW.system "computed solution 4"
    , waveFanTestGroup SW.solution5 SW.system "computed solution 5"
    ]
