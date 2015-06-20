
import Test.Tasty

import qualified Tests.Math.ConservationLaws
import qualified Tests.Math.ConservationLaws.Examples.Burgers
import qualified Tests.Math.Integration

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Math.Integration.tests
  , Tests.Math.ConservationLaws.tests
  , Tests.Math.ConservationLaws.Examples.Burgers.tests
  ]

