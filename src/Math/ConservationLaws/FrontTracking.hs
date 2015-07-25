
module Math.ConservationLaws.FrontTracking (
    Piecewise(..),
    evalPiecewise,
    PiecewiseXt(..),
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

import Data.Maybe (isJust, fromMaybe)
import qualified Data.Set as Set

import qualified Data.Vector as V

import Math.ConservationLaws
import Math.Fan
import Math.LinearAlgebra

type Piecewise = Fan Mat Double

evalPiecewise :: Piecewise -> Double -> Mat
evalPiecewise pw x = findOuterAt (compare x) pw

type PiecewiseXt = Fan Mat (Double, Double)

evalPiecewiseXt :: PiecewiseXt -> (Double, Double) -> Mat
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
        , generation :: Int
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

type GenerationInfo = V.Vector Int

data StagedDiscontinuity
  = FrontD Front
  | Break Double GenerationInfo

data Stage = Stage Double (Fan Mat StagedDiscontinuity)

piecewiseToStage :: Double -> Int -> Piecewise -> Stage
piecewiseToStage time n (Fan vec last) =
    Stage
        time
        (Fan
            (V.map (\(mat, dbl) -> (mat, Break dbl $ V.replicate n 0)) vec)
            last)

data Output =
    Output
        { fronts :: Set.Set Front
        , solution :: Fan PiecewiseXt Double
        , stage :: Stage
        }

validUntil :: Output -> Double
validUntil Output { stage = Stage time _ } = time

evaluate :: Output -> (Double, Double) -> Mat
evaluate Output{..} (x, t) =
    evalPiecewiseXt pwxt (x, (t - time))
  where
    pwxt = indexO solution i
    time = if i == 0 then 0 else indexI solution (i-1)
    i = findIndexAt (compare t) solution

data Configuration =
    Configuration
        { stopAtTime :: Maybe Double
        , stopAfterSteps :: Maybe Int
        , delta :: Double
        , maxGeneration :: Maybe Int
        , previousOutput :: Maybe Output
        , epsilon :: Double
        , initial :: Piecewise
        }

emptyPiecewise :: Piecewise
emptyPiecewise = Fan [] 0

emptyPiecewiseXt :: PiecewiseXt
emptyPiecewiseXt = Fan [] 0

trackFronts :: Configuration -> System -> Output
trackFronts config@Configuration{..} system =
    loop config system 0 $ fromMaybe newOutput previousOutput
  where
    newOutput = Output
        { fronts = Set.empty
        , solution = Fan [] emptyPiecewiseXt
        , stage = piecewiseToStage 0 (n system) initial
        }

loop :: Configuration -> System -> Int -> Output -> Output
loop config@Configuration{..} system m output@Output{..} =
    case (stopAfterSteps, stopAtTime) of
        (Just steps, _) | m >= steps -> output
        (_, Just time) | thisTime >= time -> output
        _ | nextTime == 1/0 -> Output
            { stage = nextStage
            , fronts = withEndedFronts
            , solution =
                case solution of
                    Fan solns last -> Fan solns piecewiseXt
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
        endThem i = V.map (\f -> f {end = Just (nextXs V.! i, thisTime)})

    withNewFronts :: Set.Set Front
    withNewFronts =
        insertElements fronts $ V.filter ((==thisTime) . snd . origin) $
            V.map snd currentFronts

    nextStage :: Stage
    nextStage = Stage
        nextTime
        (Fan
            (V.map stageIntersection intersected)
            (lastO fan))

    stageIntersection :: (Int, Int) -> (Mat, StagedDiscontinuity)
    stageIntersection (i, j)
      | i + 1 == j = (iM, FrontD iF)
      | otherwise =
        (iM, Break
            (nextXs V.! i)
            (V.map
                (\fam -> if isFamilyInvolved fam then 0 else sum)
                [0..n system-1]))
      where
        isFamilyInvolved i =
            isJust $ V.find ((== i) . family . snd) currentFronts
        sum = V.foldl' (+) 0 familiesMaxGen
        familiesMaxGen = V.map maxGenOfFamily [0..n system-1]
        maxGenOfFamily f = V.foldl'
            (\m Front{..} ->
                if family == f && generation > m then generation else m)
            0
            relevantFronts
        relevantFronts = V.map snd $ V.slice i (j-i) currentFronts
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
        if s <= s' then 1/0 else (nowX' - nowX) / (s - s')

    piecewiseXt :: PiecewiseXt
    piecewiseXt = Fan
        (V.map (\(mat, f) -> (mat, frontToNow f)) currentFronts)
        (indexO fan (iLength fan))

    frontToNow :: Front -> (Double, Double)
    frontToNow Front {origin = (x, t), speed = s} =
        (x + s * (thisTime - t), s)

    currentFronts :: V.Vector (Mat, Front)
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
        -> Mat
        -> Mat
        -> V.Vector (Mat, Front)
    solveDiscontinuity _ (FrontD front) uL _ = [(uL, front)]
    solveDiscontinuity thisTime (Break x gi) uL uR =
        approximateRiemann (x, thisTime) gi uL uR

    approximateRiemann
        :: (Double, Double)
        -> GenerationInfo
        -> Mat
        -> Mat
        -> V.Vector (Mat, Front)
    approximateRiemann origin' gi uL uR =
        case maxGeneration of
            Nothing -> unpruned
            Just g -> V.unfoldrN (V.length unpruned) (prune g) (0, Nothing)
      where
        prune :: Int -> (Int, Maybe Mat) -> Maybe ((Mat, Front), (Int, Maybe Mat))
        prune g (i, maybeMat) = case maybeMat of
            _ | i >= V.length unpruned -> Nothing
            Nothing
              | toKeep -> Just ((m1, front), (i+1, Nothing))
              | otherwise -> prune g (i+1, Just m1)
            Just m
              | toKeep -> Just ((m, front), (i+1, Nothing))
              | otherwise -> prune g (i+1, Just m)
          where
            (m1, front) = unpruned V.! i
            toKeep = generation front <= g

        unpruned = V.concat $ V.toList $ V.map approximateWave solnVec
        Fan solnVec _ = solveRiemann system uL uR

        approximateWave :: (Mat, Wave) -> V.Vector (Mat, Front)
        approximateWave (val, RWave r) = approximateRarefaction origin' gi r
        approximateWave (val, SWave s) =
            [(val, Front
                { origin = origin'
                , end = Nothing
                , family = sFamily s
                , speed = Math.ConservationLaws.speed s
                , frontType = SFront
                , generation = gi V.! sFamily s
                })]

    approximateRarefaction
        :: (Double, Double)
        -> GenerationInfo
        -> Rarefaction
        -> V.Vector (Mat, Front)
    approximateRarefaction origin' gi Rarefaction{..} =
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
            , generation = gi V.! rFamily
            }
