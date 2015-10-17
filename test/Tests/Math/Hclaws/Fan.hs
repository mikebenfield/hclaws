
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.Hclaws.Fan (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import qualified Math.Hclaws.Fan as F

tests = $(testGroupGenerator)

fan0 :: F.Fan Char Int
fan0 = F.Fan [('a', 0)] 'b'

case_findOuterAtBinary_0 = do
    F.findOuterAtBinary (compare 0) fan0 @?= 'a'

fan1 :: F.Fan Char Int
fan1 = F.Fan [('a', 0), ('b', 1)] 'c'

case_indexI_1 = do
    F.indexI fan1 0 @?= 0

case_findOuterAt_1 = do
    F.findOuterAt (compare 2) fan1 @?= 'c'

case_findOuterAt_2 = do
    F.findOuterAt (compare 1) fan1 @?= 'b'

case_findOuterAt_3 = do
    F.findOuterAt (compare 0) fan1 @?= 'a'

case_findOuterAtBinary_1 = do
    F.findOuterAtBinary (compare 2) fan1 @?= 'c'

case_findOuterAtBinary_2 = do
    F.findOuterAtBinary (compare 1) fan1 @?= 'b'

case_findOuterAtBinary_3 = do
    F.findOuterAtBinary (compare 0) fan1 @?= 'a'

fan2 :: F.Fan Char Int
fan2 = F.Fan [('a', 0), ('b', 2)] 'c'

case_findOuterAt_4 = do
    F.findOuterAt (compare 1) fan1 @?= 'b'
