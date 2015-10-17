{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Hclaws.Systems.Linear (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import Test.Curves

import Math.Hclaws.ConservationLaws

import Math.Hclaws.Systems.Linear as L

tests :: TestTree
tests = testGroup "Math.Hclaws.Systems.Linear" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

solution1 = solveRiemann L.system [1, 2, 3] [2, 3, 4]

solution2 = solveRiemann L.system [17, 24, 29] [-100, -1, 3124]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ waveFanTestGroup solution1 L.system "solution 1"
    , waveFanTestGroup solution2 L.system "solution 2"
    ]
