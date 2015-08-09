{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.ConservationLaws (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.TH (testGroupGenerator)

import qualified Math.ConservationLaws as CL
import qualified Math.ConservationLaws.Examples.Burgers as B

tests :: TestTree
tests = $(testGroupGenerator)
