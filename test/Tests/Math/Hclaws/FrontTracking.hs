{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.Hclaws.FrontTracking (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (assertString, Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Test.HUnitExtras

import qualified Data.Set as Set
import qualified Data.Vector as V

import Math.Hclaws.Fan
import Math.Hclaws.LinearAlgebra
import qualified Math.Hclaws.Systems.Burgers as B
import qualified Math.Hclaws.Systems.Linear as L
import qualified Math.Hclaws.Systems.ShallowWater as SW

import qualified Math.Hclaws.FrontTracking as FT

tests :: TestTree
tests = $(testGroupGenerator)

eps :: Double
eps = 0.0001

pw :: FT.Piecewise 1
pw = Fan [([0], 0)] [1]

case_evalPiecewise1 = FT.evalPiecewise pw (-1) @?= [0]

case_evalPiecewise2 = FT.evalPiecewise pw 1 @?= [1]

pwxt :: FT.PiecewiseXt 1
pwxt = Fan [([0], (0, 1))] [1]

case_evalPiecewiseXt1 = FT.evalPiecewiseXt pwxt (-1, 0) @?= [0]

case_evalPiecewiseXt2 = FT.evalPiecewiseXt pwxt (1, 0) @?= [1]

case_evalPiecewiseXt3 = FT.evalPiecewiseXt pwxt (1, 1.1) @?= [0]

simplePiecewise :: FT.Piecewise 1
simplePiecewise = Fan [] [0]

simpleConfig = FT.Configuration
    { FT.stopAtTime = Nothing
    , FT.stopAfterSteps = Nothing
    , FT.delta = 0.1
    , FT.epsilon = 0.00001
    , FT.previousOutput = Nothing
    , FT.initial = simplePiecewise
    }

output1 :: FT.Output 1
output1 = FT.trackFronts simpleConfig B.system

case_trackFronts_1_1 = Set.size (FT.fronts output1) @?= 0

case_trackFronts_1_2 =
    case FT.solution output1 of
        Fan vec _ -> V.length vec @?= 0

case_trackFronts_1_3 = FT.validUntil output1 @?= 1/0

piecewise2 :: FT.Piecewise 1
piecewise2 = Fan [([1], 0)] [0]

output2 :: FT.Output 1
output2 = FT.trackFronts simpleConfig {FT.initial = piecewise2} B.system

case_trackFronts_2_1 = Set.size (FT.fronts output2) @?= 1

case_trackFronts_2_2 =
    case FT.solution output2 of
        Fan vec _ -> V.length vec @?= 0

case_trackFronts_2_3 = FT.validUntil output2 @?= 1/0

case_trackFronts_2_4 = FT.evaluate output2 (0, 1) @?~ [1]

case_trackFronts_2_5 = FT.evaluate output2 (1, 1) @?~ [0]

piecewise3 :: FT.Piecewise 1
piecewise3 = Fan [([1], 0), ([0.5], 1)] [0]

output3 = FT.trackFronts simpleConfig {FT.initial = piecewise3} B.system

case_trackFronts_3_1 = Set.size (FT.fronts output3) @?= 3

case_trackFronts_3_2 = FT.validUntil output3 @?= 1/0

piecewise4 :: FT.Piecewise 1
piecewise4 = Fan [([0], 0)] [1]
output4 = FT.trackFronts
    simpleConfig {FT.initial = piecewise4, FT.delta = 1000} B.system

case_trackFronts_4_1 = Set.size (FT.fronts output4) @?= 1

case_trackFronts_4_2 = FT.validUntil output4 @?= 1/0

simpleConfig3 = FT.Configuration
    { FT.stopAtTime = Nothing
    , FT.stopAfterSteps = Nothing
    , FT.delta = 0.1
    , FT.epsilon = 0.00001
    , FT.previousOutput = Nothing
    , FT.initial = piecewise5
    }

piecewise5 :: FT.Piecewise 3
piecewise5 = Fan [([0, 0, 0], 0), ([2, 3, 4], 1)] [1, 7, 1]

output5 = FT.trackFronts
    simpleConfig3 {FT.initial = piecewise5, FT.stopAfterSteps = Just 2} L.system

case_trackFronts_5_1 =
    Set.size (FT.fronts output5) @?= 8
