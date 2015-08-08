
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.Integration (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (assertString, Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Test.HUnitExtras

import Data.Matrix (fromLists, (!))

import Math.Curves
import Math.LinearAlgebra

import qualified Math.Integration as I

tests :: TestTree
tests = $(testGroupGenerator)

form1 m = row [1, 1]
form2 Point{..} = row [t, x]
form3 Point{..} = fromLists [[1, 1], [t, x]]
circle1 = circle (point 1 1) 1
box1 = box (point 0 0) 1 3

acc = 0.00001

case_simpson_1 =
    I.simpson (\x -> col [x^2]) 0 1 @?~ col [1/3]
case_simpson_2 = 
    I.simpson (\x -> col [sin x]) 0 (pi/2) @?~ col [1]

case_adaptiveSimpson_1 =
    I.adaptiveSimpson acc (\x -> col [sin x]) 0 pi @?~ col [2]
case_adaptiveSimpson_2 =
    I.adaptiveSimpson acc (\x -> col [x, x^2]) 0 1 @?~ col [1/2, 1/3]

case_adaptiveSimpsonLineIntegral_1 =
    I.adaptiveSimpsonLineIntegral acc circle1 form1 0 1 @?~ col [0]
case_adaptiveSimpsonLineIntegral_2 =
    I.adaptiveSimpsonLineIntegral acc circle1 form2 0 1 @?~ col [0]
case_adaptiveSimpsonLineIntegral_3 =
    I.adaptiveSimpsonLineIntegral acc box1 form1 0 1 @?~ col [0]
case_adaptiveSimpsonLineIntegral_4 =
    I.adaptiveSimpsonLineIntegral acc box1 form2 0 1 @?~ col [0]
case_adaptiveSimpsonLineIntegral_5 =
    I.adaptiveSimpsonLineIntegral acc box1 form3 0 1 @?~ col [0, 0]

case_euler_1 =
    I.euler 500 (\x _ -> x) 1 1 @?~ col [exp 1]

case_rungeKutta_1 =
    I.rungeKutta 20 (\x _ -> x) 1 1 @?~ col [exp 1]
