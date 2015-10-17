{-# LANGUAGE TemplateHaskell #-}

module Tests.Math.Hclaws.ConservationLaws (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.TH (testGroupGenerator)

import qualified Math.Hclaws.ConservationLaws as CL
import qualified Math.Hclaws.Systems.Burgers as B

tests :: TestTree
tests = $(testGroupGenerator)
