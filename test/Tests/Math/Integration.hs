
module Tests.Math.Integration (
  tests
) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import qualified Math.Integration as I

import Math.LinearAlgebra

main = defaultMain tests

epsilon :: Double
epsilon = 0.1

infix 1 @?~
(@?~) :: (Normed v, Show v) => v -> v -> HU.Assertion
l @?~ r 
  | norm (l-.r) < epsilon = HU.assertString ""
  | otherwise = HU.assertString $ "expected: " ++ (show r) ++ " (approximately)\n" ++
                                  " but got: " ++ (show l)
infix 1 @~?
(@~?) :: (Normed v, Show v) => v -> v -> HU.Assertion
(@~?) = flip (@?~)

tests :: TestTree
tests = testGroup "ConservationLaws" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
  [ 
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [
    HU.testCase "simpson 1" $
      I.simpson (\x -> x^2) 0 1 @?~ (1/3)
  , HU.testCase "simpson 2" $
      I.simpson sin 0 pi @?~ 2
  , HU.testCase "adaptiveSimpson 1" $
      I.adaptiveSimpson 0.01 sin 0 pi @?~ 2
  , HU.testCase "adaptiveSimpson matrix 1" $
      I.adaptiveSimpson 0.01 (\x -> col [x, x^2]) 0 1 @?~ col [1/2, 1/3]
  ]

