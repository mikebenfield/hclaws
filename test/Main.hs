
import Test.Tasty

import qualified Tests.Math.Curves
import qualified Tests.Math.Integration
import qualified Tests.Math.ConservationLaws
import qualified Tests.Math.ConservationLaws.Examples.Burgers
import qualified Tests.Math.ConservationLaws.Examples.ShallowWater
import qualified Tests.Math.ConservationLaws.Examples.TwoComponentChromatography
import qualified Tests.Math.ConservationLaws.Examples.JenssenYoung2004_31

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Math.Curves.tests
  , Tests.Math.Integration.tests
  , Tests.Math.ConservationLaws.tests
  , Tests.Math.ConservationLaws.Examples.Burgers.tests
  , Tests.Math.ConservationLaws.Examples.ShallowWater.tests
  , Tests.Math.ConservationLaws.Examples.TwoComponentChromatography.tests
  , Tests.Math.ConservationLaws.Examples.JenssenYoung2004_31.tests
  ]

