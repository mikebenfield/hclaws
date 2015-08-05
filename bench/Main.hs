
{-# LANGUAGE OverloadedLists #-}

import Criterion.Main

import Data.Vector as V

import Math.Fan

vs = [1..10000]

fan1 :: Fan Int Int
fan1 = Fan (V.zip vs vs) 10001

fan_1 :: Fan Int Int
fan_1 = Fan [(0, 0)] 1

fan_2 :: Fan Int Int
fan_2 = Fan [(0, 0), (1, 1)] 2

fan_3 :: Fan Int Int
fan_3 = Fan [(0, 0), (1, 1), (2, 2)] 3

fan_4 :: Fan Int Int
fan_4 = Fan [(0, 0), (1, 1), (2, 2), (3, 3)] 4

fan_6 :: Fan Int Int
fan_6 = Fan [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5)] 5

fan_10 :: Fan Int Int
fan_10 = Fan (V.zip [0..9] [0..9]) 10

fan_16 :: Fan Int Int
fan_16 = Fan (V.zip [0..15] [0..15]) 16

fan_20 :: Fan Int Int
fan_20 = Fan (V.zip [0..19] [0..19]) 20

main = defaultMain
    [ bgroup "findOuterAt" []
        -- [ bench "1a" $ whnf (findOuterAt (compare 5000)) fan1
        -- , bench "1b" $ whnf (findOuterAt (compare 1000)) fan1
        -- , bench "1c" $ whnf (findOuterAt (compare 9000)) fan1
        -- , bench "2a" $ whnf (findOuterAtBinary (compare 5000)) fan1
        -- , bench "2b" $ whnf (findOuterAtBinary (compare 1000)) fan1
        -- , bench "2c" $ whnf (findOuterAtBinary (compare 9000)) fan1
        -- ]
    , bgroup "findOuterAt: size compare"
        [ bench "1a Linear" $ whnf (findOuterAt (compare 0)) fan_1
        , bench "1a Binary" $ whnf (findOuterAtBinary (compare 0)) fan_1
        , bench "1b Linear" $ whnf (findOuterAt (compare 1)) fan_1
        , bench "1b Binary" $ whnf (findOuterAtBinary (compare 1)) fan_1

        , bench "2a" $ whnf (findOuterAt (compare 0)) fan_2
        , bench "2a Linear" $ whnf (findOuterAtLinear (compare 0)) fan_2
        , bench "2a Binary" $ whnf (findOuterAtBinary (compare 0)) fan_2
        , bench "2b" $ whnf (findOuterAt (compare 1)) fan_2
        , bench "2b Linear" $ whnf (findOuterAtLinear (compare 1)) fan_2
        , bench "2b Binary" $ whnf (findOuterAtBinary (compare 1)) fan_2
        , bench "2c" $ whnf (findOuterAt (compare 2)) fan_2
        , bench "2c Linear" $ whnf (findOuterAtLinear (compare 2)) fan_2
        , bench "2c Binary" $ whnf (findOuterAtBinary (compare 2)) fan_2

        -- , bench "4a Linear" $ whnf (findOuterAt (compare 0)) fan_4
        -- , bench "4a Binary" $ whnf (findOuterAtBinary (compare 0)) fan_4
        -- , bench "4b Linear" $ whnf (findOuterAt (compare 1)) fan_4
        -- , bench "4b Binary" $ whnf (findOuterAtBinary (compare 1)) fan_4
        -- , bench "4c Linear" $ whnf (findOuterAt (compare 2)) fan_4
        -- , bench "4c Binary" $ whnf (findOuterAtBinary (compare 2)) fan_4
        -- , bench "4d Linear" $ whnf (findOuterAt (compare 3)) fan_4
        -- , bench "4d Binary" $ whnf (findOuterAtBinary (compare 3)) fan_4
        -- , bench "4e Linear" $ whnf (findOuterAt (compare 4)) fan_4
        -- , bench "4e Binary" $ whnf (findOuterAtBinary (compare 4)) fan_4

        -- , bench "6b Linear" $ whnf (findOuterAt (compare 1)) fan_6
        -- , bench "6b Binary" $ whnf (findOuterAtBinary (compare 1)) fan_6
        -- , bench "6d Linear" $ whnf (findOuterAt (compare 3)) fan_6
        -- , bench "6d Binary" $ whnf (findOuterAtBinary (compare 3)) fan_6
        -- , bench "6f Linear" $ whnf (findOuterAt (compare 5)) fan_6
        -- , bench "6f Binary" $ whnf (findOuterAtBinary (compare 5)) fan_6

        , bench "10a" $ whnf (findOuterAt (compare 2)) fan_10
        , bench "10a Linear" $ whnf (findOuterAtLinear (compare 2)) fan_10
        , bench "10a Binary" $ whnf (findOuterAtBinary (compare 2)) fan_10
        , bench "10b" $ whnf (findOuterAt (compare 5)) fan_10
        , bench "10b Linear" $ whnf (findOuterAtLinear (compare 5)) fan_10
        , bench "10b Binary" $ whnf (findOuterAtBinary (compare 5)) fan_10
        , bench "10c" $ whnf (findOuterAt (compare 8)) fan_10
        , bench "10c Linear" $ whnf (findOuterAtLinear (compare 8)) fan_10
        , bench "10c Binary" $ whnf (findOuterAtBinary (compare 8)) fan_10

        , bench "16a Linear" $ whnf (findOuterAt (compare 2)) fan_16
        , bench "16a Binary" $ whnf (findOuterAtBinary (compare 2)) fan_16
        , bench "16b Linear" $ whnf (findOuterAt (compare 8)) fan_16
        , bench "16b Binary" $ whnf (findOuterAtBinary (compare 8)) fan_16
        , bench "16c Linear" $ whnf (findOuterAt (compare 14)) fan_16
        , bench "16c Binary" $ whnf (findOuterAtBinary (compare 14)) fan_16

        , bench "20a Linear" $ whnf (findOuterAt (compare 5)) fan_20
        , bench "20a Binary" $ whnf (findOuterAtBinary (compare 5)) fan_20
        , bench "20b Linear" $ whnf (findOuterAt (compare 10)) fan_20
        , bench "20b Binary" $ whnf (findOuterAtBinary (compare 10)) fan_20
        , bench "20c Linear" $ whnf (findOuterAt (compare 15)) fan_20
        , bench "20c Binary" $ whnf (findOuterAtBinary (compare 15)) fan_20
        ]
    ]
