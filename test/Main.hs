
import Test.Tasty

import qualified Tests.Math.ConservationLaws
import qualified Tests.Math.ConservationLaws.FrontTracking
import qualified Tests.Math.ConservationLaws.Examples.Burgers
import qualified Tests.Math.ConservationLaws.Examples.JenssenYoung2004_31
import qualified Tests.Math.ConservationLaws.Examples.ShallowWater
import qualified Tests.Math.ConservationLaws.Examples.TwoComponentChromatography
import qualified Tests.Math.Curves
import qualified Tests.Math.Fan
import qualified Tests.Math.Integration

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Math.ConservationLaws.Examples.Burgers.tests
  , Tests.Math.ConservationLaws.Examples.JenssenYoung2004_31.tests
  , Tests.Math.ConservationLaws.Examples.ShallowWater.tests
  , Tests.Math.ConservationLaws.Examples.TwoComponentChromatography.tests
  , Tests.Math.ConservationLaws.FrontTracking.tests
  , Tests.Math.ConservationLaws.tests
  , Tests.Math.Curves.tests
  , Tests.Math.Fan.tests
  , Tests.Math.Integration.tests
  ]

