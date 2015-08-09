{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.ConservationLaws.FrontTracking (
    Piecewise,
    evalPiecewise,
    PiecewiseXt,
    evalPiecewiseXt,
    Front(..),
    FrontType(..),
    Stage,
    StagedDiscontinuity(..),
    Output(..),
    validUntil,
    evaluate,
    Configuration(..),
    trackFronts,
) where

import GHC.TypeLits

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Data.Vector as V

import Math.ConservationLaws
import Math.Fan

type Piecewise (n::Nat) = Fan (Vector n) Double

evalPiecewise :: Piecewise n -> Double -> Vector n
evalPiecewise pw x = findOuterAt (compare x) pw

type PiecewiseXt (n::Nat) = Fan (Vector n) (Double, Double)

evalPiecewiseXt :: PiecewiseXt n -> (Double, Double) -> Vector n
evalPiecewiseXt pw (x, t) = findOuterAt (\(x', s) -> compare x (x' + s*t)) pw

data FrontType = RFront | SFront
    deriving (Eq, Show)

data Front =
    Front
        { origin :: (Double, Double)
        , end :: Maybe (Double, Double)
        , family :: Int
        , speed :: Double
        , frontType :: FrontType
        }
    deriving (Show)

instance Eq Front where
    (==)
        Front { origin = (x, t), speed = s}
        Front { origin = (x', t'), speed = s'} =
        t == t' && x == x' && s == s'

instance Ord Front where
    compare
        Front {origin = (x, t), speed = s}
        Front {origin = (x', t'), speed = s'} =
        case (compare t t', compare x x', compare s s') of
            (EQ, EQ, c) -> c
            (EQ, c, _) -> c
            (c, _, _) -> c

data StagedDiscontinuity
  = FrontD Front
  | Break Double

data Stage (n::Nat) = Stage Double (Fan (Vector n) StagedDiscontinuity)

piecewiseToStage :: Double -> Piecewise n -> Stage n
piecewiseToStage time (Fan vec last) =
    Stage
        time
        (Fan
            (V.map (\(mat, dbl) -> (mat, Break dbl)) vec)
            last)

data Output (n::Nat) =
    Output
        { fronts :: Set.Set Front
        , solution :: Fan (PiecewiseXt n) Double
        , stage :: Stage n
        }

validUntil :: Output n -> Double
validUntil Output { stage = Stage time _ } = time

evaluate :: Output n -> (Double, Double) -> Vector n
evaluate Output{..} (x, t) =
    evalPiecewiseXt pwxt (x, (t - time))
  where
    pwxt = indexO solution i
    time = if i == 0 then 0 else indexI solution (i-1)
    i = findIndexAt (compare t) solution

data Configuration (n::Nat) =
    Configuration
        { stopAtTime :: Maybe Double
        , stopAfterSteps :: Maybe Int
        , delta :: Double
        , previousOutput :: Maybe (Output n)
        , epsilon :: Double
        , initial :: Piecewise n
        }

trackFronts :: Configuration n -> System n -> Output n
trackFronts config@Configuration{..} system =
    loop config system 0 $ fromMaybe newOutput previousOutput
  where
    newOutput = Output
        { fronts = Set.empty
        , solution = Fan [] undefined
        , stage = piecewiseToStage 0 initial
        }

loop
    :: forall (n::Nat)
    . Configuration n
    -> System n
    -> Int
    -> Output n
    -> Output n
loop config@Configuration{..} system m output@Output{..} =
    case (stopAfterSteps, stopAtTime) of
        (Just steps, _) | m >= steps -> output
        (_, Just time) | thisTime >= time -> output
        _ | nextTime == 1/0 -> Output
            { stage = nextStage
            , fronts = withEndedFronts
            , solution =
                case solution of
                    Fan solns _ -> Fan solns piecewiseXt
            }
        _ -> loop config system (m+1) $ Output
            { stage = nextStage
            , fronts = withEndedFronts
            , solution =
                case solution of
                    Fan solns last ->
                        Fan (V.snoc solns (piecewiseXt, nextTime)) last
            }
  where
    Stage thisTime fan = stage

    insertElements :: Set.Set Front -> V.Vector Front -> Set.Set Front
    insertElements set vec = V.foldl' (flip Set.insert) set vec

    withEndedFronts :: Set.Set Front
    withEndedFronts =
        V.foldl'
            (\fronts_ (i, j) ->
                if i + 1 == j
                   then fronts_
                   else insertElements fronts_ $
                    endThem i $ V.slice i (j-i) $ V.map snd currentFronts)
            withNewFronts
            intersected
      where
        endThem i = V.map (\f -> f {end = Just (nextXs V.! i, nextTime)})

    withNewFronts :: Set.Set Front
    withNewFronts =
        insertElements fronts $ V.filter ((==thisTime) . snd . origin) $
            V.map snd currentFronts

    nextStage :: Stage n
    nextStage = Stage
        nextTime
        (Fan
            (V.map stageIntersection intersected)
            (lastO fan))

    stageIntersection :: (Int, Int) -> (Vector n, StagedDiscontinuity)
    stageIntersection (i, j)
      | i + 1 == j = (iM, FrontD iF)
      | otherwise =
        (iM, Break (nextXs V.! i))
      where
        (iM, iF) = currentFronts V.! i

    -- If (i, j) is in intersected, then the indices of currentFronts
    -- in [i, j) all intersect at the same x
    intersected :: V.Vector (Int, Int)
    intersected = V.unfoldrN (V.length nextXs) uf (0, 1)

    uf :: (Int, Int) -> Maybe ((Int, Int), (Int, Int))
    uf (i, j)
      | i >= V.length nextXs = Nothing
      | j < V.length nextXs && nextXs V.! j - nextXs V.! i < epsilon
        = uf (i, j+1)
      | otherwise = Just ((i, j), (j, j+1))

    nextXs :: V.Vector Double
    nextXs = V.map
        (\(_, Front{origin = (x, t), speed = s}) -> x + s * (nextTime - t))
        currentFronts

    nextTime :: Double
    nextTime = V.foldl' min (1/0) intersectionTimes

    intersectionTimes :: V.Vector Double
    intersectionTimes =
        V.map
            (\i -> intersectionTime
                (snd $ currentFronts V.! i)
                (snd $ currentFronts V.! (i+1)))
            [0..V.length currentFronts - 2]

    intersectionTime :: Front -> Front -> Double
    intersectionTime
        (Front {origin = (x, t), speed = s})
        (Front {origin = (x', t'), speed = s'}) =
        let nowX = x + s * (thisTime - t)
            nowX' = x' + s' * (thisTime - t')
        in
        if s <= s' then 1/0 else thisTime + (nowX' - nowX) / (s - s')

    piecewiseXt :: PiecewiseXt n
    piecewiseXt = Fan
        (V.map (\(mat, f) -> (mat, frontToNow f)) currentFronts)
        (indexO fan (iLength fan))

    frontToNow :: Front -> (Double, Double)
    frontToNow Front {origin = (x, t), speed = s} =
        (x + s * (thisTime - t), s)

    currentFronts :: V.Vector (Vector n, Front)
    currentFronts =
        V.concat $ V.toList $ V.map
            (\i -> solveDiscontinuity thisTime
                (indexI fan i)
                (indexO fan i)
                (indexO fan (i+1)))
            [0..iLength fan - 1]

    solveDiscontinuity
        :: Double
        -> StagedDiscontinuity
        -> Vector n
        -> Vector n
        -> V.Vector (Vector n, Front)
    solveDiscontinuity _ (FrontD front) uL _ = [(uL, front)]
    solveDiscontinuity thisTime (Break x) uL uR =
        approximateRiemann (x, thisTime) uL uR

    approximateRiemann
        :: (Double, Double)
        -> Vector n
        -> Vector n
        -> V.Vector (Vector n, Front)
    approximateRiemann origin' uL uR =
        V.concat $ V.toList $ V.map approximateWave solnVec
      where
        Fan solnVec _ = solveRiemann system uL uR

        approximateWave :: (Vector n, Wave n) -> V.Vector (Vector n, Front)
        approximateWave (_, RWave r) = approximateRarefaction origin' r
        approximateWave (val, SWave s) =
            [(val, Front
                { origin = origin'
                , end = Nothing
                , family = sFamily s
                , speed = Math.ConservationLaws.speed s
                , frontType = SFront
                })]

    approximateRarefaction
        :: (Double, Double)
        -> Rarefaction n
        -> V.Vector (Vector n, Front)
    approximateRarefaction origin' Rarefaction{..} =
        V.zip vals fronts
      where
        speeds = V.generate (k+1) (\i -> speedL + deltaHat * fromIntegral i)
        fronts = V.map frontOfSpeed speeds
        vals = V.map function speeds
        -- this computation of deltaHat can probably be improved
        -- or, another option: just don't compute it, and use delta
        k :: Int
        k = round $ (speedR - speedL) / delta
        deltaHat = if k == 0 then 0 else (speedR - speedL) / fromIntegral k
        frontOfSpeed s = Front
            { origin = origin'
            , end = Nothing
            , family = rFamily
            , speed = s
            , frontType = RFront
            }
