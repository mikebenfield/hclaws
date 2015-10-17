{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Hclaws.Systems.Burgers (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.Curves

import qualified Math.Hclaws.ConservationLaws as CL
import qualified Math.Hclaws.Systems.Burgers as B

tests :: TestTree
tests = testGroup "Math.Hclaws.Systems.Burgers" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ waveFanTestGroup B.solution1 B.system "solution 1"
    , waveFanTestGroup B.solution2 B.system "solution 2"
    , waveFanTestGroup B.solution3 B.system "solution 3"
    , waveFanTestGroup B.solution4 B.system "solution 4"
    , waveFanTestGroup (CL.solveRiemann B.system [-1] [1]) B.system "Example 1"
    , waveFanTestGroup (CL.solveRiemann B.system [13] [0]) B.system "Example 2"
    ]
