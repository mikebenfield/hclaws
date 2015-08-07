
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.ConservationLaws.FrontTracking (
    tests
) where

import Debug.Trace -- XXX

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (assertString, Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.Set as Set
import qualified Data.Vector as V

import Math.Fan
import Math.LinearAlgebra
import qualified Math.ConservationLaws.Examples.Burgers as B
import qualified Math.ConservationLaws.Examples.Linear as L
import qualified Math.ConservationLaws.Examples.ShallowWater as SW

import qualified Math.ConservationLaws.FrontTracking as FT

tests :: TestTree
tests = $(testGroupGenerator)

infix 1 @?~
(@?~) :: Mat -> Mat -> Assertion
l @?~ r
  | normP (1/0) (l-r) < eps = assertString ""
  | otherwise =
        assertString $
            "expected (approximately):\n" ++ (show r) ++ "\n"
         ++ " but got:\n" ++ (show l)

infix 1 @~?
(@~?) :: Mat -> Mat -> Assertion
(@~?) = flip (@?~)

eps :: Double
eps = 0.0001

pw :: FT.Piecewise
pw = Fan [(0, 0)] 1

case_evalPiecewise1 = do
    FT.evalPiecewise pw (-1) @?= 0

case_evalPiecewise2 = do
    FT.evalPiecewise pw 1 @?= 1

pwxt :: FT.PiecewiseXt
pwxt = Fan [(0, (0, 1))] 1

case_evalPiecewiseXt1 = do
    FT.evalPiecewiseXt pwxt (-1, 0) @?= 0

case_evalPiecewiseXt2 = do
    FT.evalPiecewiseXt pwxt (1, 0) @?= 1

case_evalPiecewiseXt3 = do
    FT.evalPiecewiseXt pwxt (1, 1.1) @?= 0

simplePiecewise :: FT.Piecewise
simplePiecewise = Fan [] 0

simpleConfig = FT.Configuration
    { FT.stopAtTime = Nothing
    , FT.stopAfterSteps = Nothing
    , FT.delta = 0.1
    , FT.epsilon = 0.00001
    , FT.maxGeneration = Nothing
    , FT.previousOutput = Nothing
    , FT.initial = simplePiecewise
    }

output1 :: FT.Output
output1 = FT.trackFronts simpleConfig B.system

case_trackFronts_1_1 = do
    Set.size (FT.fronts output1) @?= 0

case_trackFronts_1_2 = do
    case FT.solution output1 of
        Fan vec _ -> V.length vec @?= 0

case_trackFronts_1_3 = do
    FT.validUntil output1 @?= 1/0

piecewise2 = Fan [(1, 0)] 0

output2 :: FT.Output
output2 = FT.trackFronts simpleConfig {FT.initial = piecewise2} B.system

case_trackFronts_2_1 = do
    Set.size (FT.fronts output2) @?= 1

case_trackFronts_2_2 = do
    case FT.solution output2 of
        Fan vec _ -> V.length vec @?= 0

case_trackFronts_2_3 = do
    FT.validUntil output2 @?= 1/0

case_trackFronts_2_4 = do
    FT.evaluate output2 (0, 1) @?~ 1

case_trackFronts_2_5 = do
    FT.evaluate output2 (1, 1) @?~ 0

piecewise3 = Fan [(1, 0), (col [0.5], 1)] 0

output3 = FT.trackFronts simpleConfig {FT.initial = piecewise3} B.system

case_trackFronts_3_1 = do
    Set.size (FT.fronts output3) @?= 3

case_trackFronts_3_2 = do
    FT.validUntil output3 @?= 1/0

piecewise4 = Fan [(0, 0)] 1
output4 = FT.trackFronts
    simpleConfig {FT.initial = piecewise4, FT.delta = 1000} B.system

case_trackFronts_4_1 = do
    Set.size (FT.fronts output4) @?= 1

case_trackFronts_4_2 = do
    FT.validUntil output4 @?= 1/0

piecewise5 = Fan [(col [0, 0, 0], 0), (col [2, 3, 4], 1)] (col [1, 7, 1])
output5 = FT.trackFronts
    simpleConfig {FT.initial = piecewise5, FT.stopAfterSteps = Just 2} L.system

case_trackFronts_5_1 =
    Set.size (FT.fronts output5) @?= 8
