{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Curves (
    waveFanTestGroup,
) where

import GHC.TypeLits (KnownNat)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU

import Test.HUnitExtras

import Math.FTensor.Algebra

import Math.Hclaws.Curves
import Math.Hclaws.LinearAlgebra
import qualified Math.Hclaws.ConservationLaws as CL

-- a list of arbitrary curves to test solutions on
curves :: [Curve]
curves =
    [ box (point (-5) 1) 20 20
    , box (point (-10) 2) 50 3
    , box (point (-30) 3) 100 20
    , box (point 0 0.2) 1 1
    , box (point 1 0.1) 1 1
    , box (point 2 0.1) 3 3
    , circle (point 0 5) 4
    , circle (point (-1) 2) 1
    , circle (point 1 2) 1
    , circle (point (-2) 3) 2.9
    , circle (point 2 3) 2.9
    ]

waveFanTestGroup
    :: KnownNat n
    => CL.WaveFan n
    -> CL.System n
    -> String
    -> TestTree
waveFanTestGroup wf system name =
    testGroup name $ zipWith testCurve [0..] curves
  where
    testCurve :: Int -> Curve -> TestTree
    testCurve i c =
        HU.testCase ("curve " ++ show i) $
            CL.integrateFanOnCurve c system wf @?~ zero
