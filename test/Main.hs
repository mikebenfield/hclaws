
import Test.Tasty

import qualified Tests.Math.Hclaws.ConservationLaws
import qualified Tests.Math.Hclaws.FrontTracking
import qualified Tests.Math.Hclaws.Systems.Burgers
import qualified Tests.Math.Hclaws.Systems.JenssenYoung2004_31
import qualified Tests.Math.Hclaws.Systems.Linear
import qualified Tests.Math.Hclaws.Systems.ShallowWater
import qualified Tests.Math.Hclaws.Systems.TveitoWinther1995_3
import qualified Tests.Math.Hclaws.Systems.TwoComponentChromatography
import qualified Tests.Math.Hclaws.Curves
import qualified Tests.Math.Hclaws.Differentiation
import qualified Tests.Math.Hclaws.Fan
import qualified Tests.Math.Hclaws.Integration

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Math.Hclaws.Systems.Burgers.tests
  , Tests.Math.Hclaws.Systems.JenssenYoung2004_31.tests
  , Tests.Math.Hclaws.Systems.Linear.tests
  , Tests.Math.Hclaws.Systems.ShallowWater.tests
  , Tests.Math.Hclaws.Systems.TwoComponentChromatography.tests
  , Tests.Math.Hclaws.Systems.TveitoWinther1995_3.tests
  , Tests.Math.Hclaws.FrontTracking.tests
  , Tests.Math.Hclaws.ConservationLaws.tests
  , Tests.Math.Hclaws.Curves.tests
  , Tests.Math.Hclaws.Differentiation.tests
  , Tests.Math.Hclaws.Fan.tests
  , Tests.Math.Hclaws.Integration.tests
  ]
