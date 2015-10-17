{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Hclaws.Systems.JenssenYoung2004_31 (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.Curves

import qualified Math.Hclaws.Systems.JenssenYoung2004_31 as JY

tests :: TestTree
tests =
    testGroup "Math.ConservationLaws.Examples.JenssenYoung2004_31"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [
    ]
