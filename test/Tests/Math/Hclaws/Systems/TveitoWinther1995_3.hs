{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.Hclaws.Systems.TveitoWinther1995_3 (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Test.Curves

import Math.Hclaws.ConservationLaws
import qualified Math.Hclaws.Systems.TveitoWinther1995_3 as TW

tests :: TestTree
tests =
    testGroup "Math.Hclaws.Systems.TveitoWinther1995_3"
        [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "UnitTests" $
    [ waveFanTestGroup
        (solveRiemann TW.system [0.3, 1] [0.2, 1.2])
        TW.system
        "left region 1"
    , waveFanTestGroup
        (solveRiemann TW.system [0.3, 1] [0.9, 0.5])
        TW.system
        "left region 2"
    , waveFanTestGroup
        (solveRiemann TW.system [0.3, 1] [0.2, 0.05])
        TW.system
        "left region 3"
    , waveFanTestGroup
        (solveRiemann TW.system [0.7, 1] [0.8, 0.5])
        TW.system
        "right region 2"
    , waveFanTestGroup
        (solveRiemann TW.system [0.7, 1] [0.1, 1.2])
        TW.system
        "right region 1"
    , waveFanTestGroup
        (solveRiemann TW.system [0.7, 1] [0.1, 0.1])
        TW.system
        "right region 3"
    , waveFanTestGroup
        (solveRiemann TW.system [0.5, 0.5] [0.5, 0.5])
        TW.system
        "constant"
    ]
