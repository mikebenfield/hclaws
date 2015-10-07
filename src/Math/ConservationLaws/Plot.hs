{-# LANGUAGE DataKinds #-}

module Math.ConservationLaws.Plot (
    plotFronts,
    samplePlotFronts,
    output,
    asString
) where

import Data.Proxy
import qualified Data.Set as Set
import GHC.Exts (fromList)

import Graphics.EasyPlot

import qualified Math.FTensor.General as F

import Math.ConservationLaws
import Math.ConservationLaws.FrontTracking
import Math.Fan
import qualified Math.ConservationLaws.Examples.ShallowWater as SW
import qualified Math.ConservationLaws.Examples.TveitoWinther1995_3 as TW

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

delta' = 0.1

initData :: Double -> [(Vector 2, Double)]
initData i = ([0.5, i], i) : initData (i + delta')

init' = Fan (fromList $ takeWhile ((<= 1-delta') . snd) (initData delta')) [0.5, 1-delta']

config = Configuration
    { stopAtTime = Just 2
    , stopAfterSteps = Nothing
    , delta = 0.2
    , epsilon = 0.00001
    , previousOutput = Nothing
    , initial = init'
        --Fan [([0.5, 0.1], 0), ([0.5, 0.2], 0.2)] [0.5, 0.3]
    }

output = trackFronts config TW.system

ts = [0.00, 0.01 .. 1]
us = map (evaluate output) (zip (repeat 0.5) ts)

asString = concatMap (\(t, u) ->
    show t ++ ", " ++ show (F.pIndex u (Proxy::Proxy '[0])) ++ "\n") $ zip ts us

plotFronts :: [Front] -> IO Bool
plotFronts fs =
    plot Aqua $ map frontToData fs

samplePlotFronts = plotFronts (Set.toList $ fronts output)
