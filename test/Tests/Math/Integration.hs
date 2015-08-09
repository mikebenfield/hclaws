{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Math.Integration (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (assertString, Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Test.HUnitExtras

import Math.FTensor.General as F
import Math.Curves
import Math.LinearAlgebra

import qualified Math.Integration as I

tests :: TestTree
tests = $(testGroupGenerator)

form1 :: Point -> F.TensorBoxed '[1, 2] Double
form1 _ = [[1, 1]]
form2 :: Point -> F.TensorBoxed '[1, 2] Double
form2 Point{..} = [[t, x]]
form3 :: Point -> F.TensorBoxed '[2, 2] Double
form3 Point{..} = [[1, 1], [t, x]]

circle1 = circle (point 1 1) 1
box1 = box (point 0 0) 1 3

acc = 0.00001

case_simpson_1 =
    I.simpson (\(x::Double) -> x^(2::Int)) 0 1 @?~ (1/3)
case_simpson_2 = 
    I.simpson (\x -> sin x) 0 (pi/2) @?~ 1

case_adaptiveSimpson_1 =
    I.adaptiveSimpson acc (\(x::Double) -> sin x) 0 pi @?~ 2
case_adaptiveSimpson_2 =
    let rhs :: F.TensorBoxed '[2] Double
        rhs = [1/2, 1/3]
    in
    I.adaptiveSimpson acc (\x -> [x, x^2]) 0 1 @?~ rhs

case_adaptiveSimpsonLineIntegral_1 =
    I.adaptiveSimpsonLineIntegral acc circle1 form1 0 1 @?~ [0]
case_adaptiveSimpsonLineIntegral_2 =
    I.adaptiveSimpsonLineIntegral acc circle1 form2 0 1 @?~ [0]
case_adaptiveSimpsonLineIntegral_3 =
    I.adaptiveSimpsonLineIntegral acc box1 form1 0 1 @?~ [0]
case_adaptiveSimpsonLineIntegral_4 =
    I.adaptiveSimpsonLineIntegral acc box1 form2 0 1 @?~ [0]
case_adaptiveSimpsonLineIntegral_5 =
    I.adaptiveSimpsonLineIntegral acc box1 form3 0 1 @?~ [0, 0]

case_euler_1 =
    I.euler 500 (\(x::Double) _ -> x) 1 1 @?~ exp 1

case_rungeKutta_1 =
    I.rungeKutta 20 (\(x::Double) _ -> x) 1 1 @?~ exp 1
