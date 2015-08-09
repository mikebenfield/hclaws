{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Math.Differentiation (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit (assertString, Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import Test.HUnitExtras

import qualified Math.Differentiation as D

tests :: TestTree
tests = $(testGroupGenerator)

-- case_diff_1 = D.diff sin 0 @?~= 1

-- f :: Scalar a => a -> Vec a
-- f x = [x*x, x*x*x]

-- case_tangent_1 =
--     let res = D.tangent f 2 in
--         norm (res -. [4, 12]) @?~= 0

-- g :: Scalar a => Vec a -> a
-- g x = (x ! 0)*(x ! 1)

-- case_gradient_1 =
--     let res = D.gradient g [2, 4] in
--         norm (res -. [4, 2]) @?~= 0
