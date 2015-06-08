
import Test.Tasty

import qualified Tests.Math.ConservationLaws

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [Tests.Math.ConservationLaws.tests]

