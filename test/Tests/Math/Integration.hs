
{-# LANGUAGE RecordWildCards #-}

module Tests.Math.Integration (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import Test.HUnitExtras

import Data.Matrix (fromLists, (!))

import Math.Curves
import Math.LinearAlgebra

import qualified Math.Integration as I

tests :: TestTree
tests = testGroup "Math.Integration" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

form1 m = row [1, 1]
form2 Point{..} = row [t, x]
form3 Point{..} = fromLists [[1, 1], [t, x]]
circle1 = circle (point 1 1) 1
box1 = box (point 0 0) 1 3

acc = 0.00001

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ HU.testCase "simpson 1" $
          I.simpson (\x -> col [x^2]) 0 1 @?~ col [1/3]
    , HU.testCase "simpson 2" $
          I.simpson (\x -> col [sin x]) 0 (pi/2) @?~ col [1]
    , HU.testCase "adaptiveSimpson 1" $
          I.adaptiveSimpson acc (\x -> col [sin x]) 0 pi @?~ col [2]
    , HU.testCase "adaptiveSimpson 2" $
          I.adaptiveSimpson acc (\x -> col [x, x^2]) 0 1 @?~ col [1/2, 1/3]
    , HU.testCase "adaptiveSimpsonLineIntegral 1" $
          I.adaptiveSimpsonLineIntegral
              acc circle1 form1 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 2" $
          I.adaptiveSimpsonLineIntegral
              acc circle1 form2 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 3" $
          I.adaptiveSimpsonLineIntegral
              acc box1 form1 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 4" $
          I.adaptiveSimpsonLineIntegral
              acc box1 form2 0 1 @?~ col [0]
    , HU.testCase "adaptiveSimpsonLineIntegral 5" $
          I.adaptiveSimpsonLineIntegral
              acc box1 form3 0 1 @?~ col [0, 0]
    ]
