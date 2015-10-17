
{-# LANGUAGE TemplateHaskell #-}

module Math.Hclaws.Differentiation (
    -- diffVal,
    -- diff,
    -- tangentVal,
    -- tangent,
    -- gradientVal,
    -- gradient
) where

-- import Prelude hiding (map)

-- import qualified Data.Vector.Unboxed as U
-- import Data.Vector.Unboxed.Deriving (derivingUnbox)

-- import Math.LinearAlgebra2

-- data D =
--     D
--         {-# UNPACK #-} !Double
--         {-# UNPACK #-} !Double
--     deriving (Eq, Ord, Show)

-- derivingUnbox "D"
--     [t| D -> (Double, Double) |]
--     [| \(D v v') -> (v, v') |]
--     [| \(v, v') -> D v v' |]

-- sq :: Num a => a -> a
-- sq x = x*x
-- {-# INLINE sq #-}

-- instance Num D where
--     D v v' + D w w' = D (v+w) (v'+w')
--     D v v' - D w w' = D (v-w) (v'-w')
--     D v v' * D w w' = D (v*w) (v*w' + w*v')
--     abs (D v v') = D (abs v) (v' * signum v)
--     signum (D v _) = D (signum v) 0
--     fromInteger i = D (fromInteger i) 0

-- instance Fractional D where
--     D v v' / D w w' = D (v/w) ((v*w' - w*v') / sq w)
--     recip (D v v') = D (recip v) (-v' / sq v)
--     fromRational r = D (fromRational r) 0

-- instance Floating D where
--     pi = D pi 0
--     exp (D v v') =
--         let ev = exp v
--         in
--         D ev (v' * ev)
--     log (D v v') = D (log v) (v' / v)
--     sqrt (D v v') =
--         let s = sqrt v
--         in
--         D s (v' / (2 * s))
--     D v v' ** D w w' =
--         let pow = v**w
--         in
--         D pow (pow*(v'*w/v + w'*log v))
--     logBase (D v v') (D w w') =
--         D (logBase v w) (v' / (v * log w) - w' * log v / (w * sq (log w)))
--     sin (D v v') = D (sin v) (v' * cos v)
--     cos (D v v') = D (cos v) (-v' * sin v)
--     tan (D v v') = D (tan v) (v' / sq (cos v))
--     asin (D v v') = D (asin v) (v' / sqrt (1 - sq v))
--     acos (D v v') = D (acos v) (-v' / sqrt (1 - sq v))
--     atan (D v v') = D (atan v) (v' / (1 + sq v))
--     sinh (D v v') = D (sinh v) (v' * cosh v)
--     cosh (D v v') = D (cosh v) (v' * sinh v)
--     tanh (D v v') = D (tanh v) (v' / sq (cosh v))
--     asinh (D v v') = D (asinh v) (v' / sqrt (1 + sq v))
--     acosh (D v v') = D (acosh v) (-v' / sqrt (1 + sq v))
--     atanh (D v v') = D (atanh v) (v' / (1 - sq v))

-- diffVal :: (D -> D) -> (Double -> (Double, Double))
-- diffVal f = \x -> case f (D x 1) of D val d -> (val, d)
-- {-# INLINE diffVal #-}

-- diff :: (D -> D) -> (Double -> Double)
-- diff f = snd . diffVal f
-- {-# INLINE diff #-}

-- tangentVal :: (D -> Vec D) -> (Double -> (Vec Double, Vec Double))
-- tangentVal f = \x ->
--     let vs = f (D x 1)
--     in
--     (map (\(D v _) -> v) vs, map (\(D _ v') -> v') vs)

-- tangent :: (D -> Vec D) -> (Double -> Vec Double)
-- tangent f = snd . tangentVal f
-- {-# INLINE tangent #-}

-- gradientVal :: (Vec D -> D) -> (Vec Double -> (Double, Vec Double))
-- gradientVal f = \v ->
--     let v' = vector v
--         partial i =
--             f $ vec $ U.imap (\j x -> if i == j then D x 1 else D x 0) v'
--         results = U.generate (U.length v') partial
--     in
--     (case U.head results of D w _ -> w, vec $ U.map (\(D _ w') -> w') results)

-- gradient :: (Vec D -> D) -> (Vec Double -> Vec Double)
-- gradient f = snd . gradientVal f
-- {-# INLINE gradient #-}
