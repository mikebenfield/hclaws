module Math.ConservationLaws.Plot (
    plotFronts,
    samplePlotFronts
) where

import qualified Data.Set as Set

import Graphics.EasyPlot

import Math.ConservationLaws.FrontTracking
import Math.Fan
import qualified Math.ConservationLaws.Examples.ShallowWater as SW

frontToColor :: Front -> Color
frontToColor Front {family = 0} = Red
frontToColor Front {family = 1} = Blue
frontToColor Front {family = 2} = Green
frontToColor _ = Black

frontToEnd :: Double -> Front -> (Double, Double)
frontToEnd maxTime Front{end = Just pt} | snd pt <= maxTime = pt
frontToEnd maxTime Front{..} =
    (fst origin + (maxTime - snd origin) * speed, maxTime)

frontToData :: Front -> Graph2D Double Double
frontToData f =
    Data2D [Style Lines, Color (frontToColor f)] [] [origin f, frontToEnd 2 f]

config = Configuration
    { stopAtTime = Just 2
    , stopAfterSteps = Nothing
    , delta = 0.2
    , epsilon = 0.00001
    , previousOutput = Nothing
    , initial =
        Fan [([1, 0], 0), ([3, 4], 1)] ([5, 1])
    }

output = trackFronts config SW.system

plotFronts :: [Front] -> IO Bool
plotFronts fs =
    plot Aqua $ map frontToData fs

samplePlotFronts = plotFronts (Set.toList $ fronts output)
