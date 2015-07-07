
module Tests.Math.ConservationLaws.Separated (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU

import qualified Math.ConservationLaws.Separated as S

import qualified Data.Vector as V

s :: S.Separated V.Vector Int V.Vector String
(Just s) =
    S.separated [Left 0, Right "a", Left 1, Right "b", Left 2, Right "c", Left 3]

sCollapsed :: S.Separated V.Vector Int V.Vector Double
(Just sCollapsed) = S.separated [Left 0, Right 1.0, Left 3]

collapser :: Int -> (Int, Double)
collapser 0 = (2, 1.0)
collapser x = error ("collapser received " ++ show x)

sPartiallyCollapsed :: S.Separated V.Vector Int V.Vector Double
(Just sPartiallyCollapsed) = S.separated [Left 0, Right 1.0, Left 2, Right 2.0, Left 3]

partialCollapser :: Int -> (Int, Double)
partialCollapser 0 = (1, 1.0)
partialCollapser 2 = (2, 2.0)
partialCollapser x = error ("partialCollapser received " ++ show x)

sSmall :: S.Separated V.Vector Int V.Vector String
(Just sSmall) = S.separated [Left 0]

tests :: TestTree
tests = testGroup "Math.ConservationLaws.Separated" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" $
    [ HU.testCase "firstVal" $
        S.firstVal s HU.@?= 0
    , HU.testCase "lastVal" $
        S.lastVal s HU.@?= 3
    , HU.testCase "length" $
        S.length s HU.@?= 3
    , HU.testCase "before" $
        S.before s 1 HU.@?= 1
    , HU.testCase "after" $
        S.after s 1 HU.@?= 2
    , HU.testCase "!" $
        (S.!) s 1 HU.@?= "b"
    , HU.testCase "collapseSeps 1" $
        S.collapseSeps s collapser HU.@?= sCollapsed
    , HU.testCase "collapseSeps 2" $
        S.collapseSeps s partialCollapser HU.@?= sPartiallyCollapsed
    , HU.testCase "collapseSeps 3" $
        S.collapseSeps sSmall undefined HU.@?= sSmall
    ]

