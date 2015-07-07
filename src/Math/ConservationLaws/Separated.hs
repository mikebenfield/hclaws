
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Math.ConservationLaws.Separated (
    SeparatedC(..),
    Separated,

    interweave, separated, length, firstVal, lastVal,
    before, maybeBefore, unsafeBefore, after, maybeAfter, unsafeAfter,
    (!), (!?), unsafeIndex, collapseSeparators
) where

import Prelude hiding (length)

import Control.Monad.ST

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

class (G.Vector (ValVector s) (Val s), G.Vector (SepVector s) (Sep s)) =>
    SeparatedC s where
    type Val s
    type Sep s
    type ValVector s :: * -> *
    type SepVector s :: * -> *
    unsafeCreate :: (ValVector s (Val s)) -> (SepVector s (Sep s)) -> s
    vals :: s -> (ValVector s (Val s))
    seps :: s -> (SepVector s (Sep s))

data Separated valueVector value separatorVector separator =
    Separated (valueVector value) (separatorVector separator)
    deriving Eq

instance (G.Vector valueVector value, G.Vector separatorVector separator)
    => SeparatedC (Separated valueVector value separatorVector separator) where
    type Val (Separated valueVector value separatorVector separator) =
        value
    type Sep (Separated valueVector value separatorVector separator) =
        separator
    type ValVector (Separated valueVector value separatorVector separator) =
        valueVector
    type SepVector (Separated valueVector value separatorVector separator) =
        separatorVector
    unsafeCreate = Separated
    vals (Separated vals' _) = vals'
    seps (Separated _ seps') = seps'

instance (G.Vector valueVector value, G.Vector separatorVector separator,
         Show value, Show separator)
    => Show (Separated valueVector value separatorVector separator) where
    show s = "SeparatedC [" ++ exceptLast ++ show (lastVal s) ++ "]"
      where
        exceptLast = concat $ map f [0..length s - 1]
        f i = show (before s i) ++ ", " ++ show (s ! i) ++ ", "

interweave 
    :: SeparatedC s
    => (ValVector s) (Val s)
    -> (SepVector s) (Sep s)
    -> Maybe s
interweave vs ss
  | G.length vs == G.length ss + 1 = Just $ unsafeCreate vs ss
  | otherwise = Nothing

separated :: SeparatedC s => [Either (Val s) (Sep s)] -> Maybe s
separated list = do
    (vals', seps') <- separated' list
    return $ unsafeCreate (G.fromList vals') (G.fromList seps')

separated' :: [Either valueT separatorT] -> Maybe ([valueT], [separatorT])
separated' [Left value] = Just ([value], [])
separated' (Left value : Right separator : rst) = do
    (values, separators) <- separated' rst
    return (value:values, separator:separators)
separated' _ = Nothing

length :: SeparatedC s => s -> Int
length = G.length . seps

firstVal :: SeparatedC s => s -> Val s
firstVal = G.unsafeHead . vals

lastVal :: SeparatedC s => s -> Val s
lastVal = G.unsafeLast . vals

before :: SeparatedC s => s -> Int -> Val s
before s i = vals s G.! i

maybeBefore :: SeparatedC s => s -> Int -> Maybe (Val s)
maybeBefore s i = vals s G.!? i

unsafeBefore :: SeparatedC s => s -> Int -> Val s
unsafeBefore s i = G.unsafeIndex (vals s) i

after :: SeparatedC s => s -> Int -> Val s
after s i = vals s G.! (i+1)

maybeAfter :: SeparatedC s => s -> Int -> Maybe (Val s)
maybeAfter s i = vals s G.!? (i+1)

unsafeAfter :: SeparatedC s => s -> Int -> Val s
unsafeAfter s i = G.unsafeIndex (vals s) (i+1)

infixl 9 !
(!) :: SeparatedC s => s -> Int -> Sep s
s ! i = seps s G.! i

infixl 9 !?
(!?) :: SeparatedC s => s -> Int -> Maybe (Sep s)
s !? i = seps s G.!? i

unsafeIndex :: SeparatedC s => s -> Int -> Sep s
unsafeIndex s i = G.unsafeIndex (seps s) i

collapseSeparators
    :: (SeparatedC s, SeparatedC t, Val s ~ Val t)
    => s
    -> (Int -> (Int, Sep t))
    -> t
collapseSeparators s f = runST $ do
    newSeps <- GM.new $ length s
    newVals <- GM.new $ length s + 1
    (_, lenNewSeps) <- whileDo
        (\(iOldSeps, iNewSeps) ->
            let (iOldSepsUpdate, newSep) = f iOldSeps
                iOldSepsUpdate' = clamp iOldSepsUpdate iOldSeps (length s - 1)
            in do
            GM.write newSeps iNewSeps newSep
            GM.write newVals iNewSeps (before s iOldSeps)
            return (iOldSepsUpdate' + 1, iNewSeps + 1)
        )
        (\(iOldSeps, _) -> iOldSeps < length s)
        (0, 0)
    GM.write newVals lenNewSeps (lastVal s)
    frozenNewSeps <- G.freeze $ GM.slice 0 lenNewSeps newSeps 
    frozenNewVals <- G.freeze $ GM.slice 0 (lenNewSeps + 1) newVals 
    return $ unsafeCreate (G.force frozenNewVals) (G.force frozenNewSeps)

whileDo :: Monad m => (a -> m a) -> (a -> Bool) -> a -> m a
whileDo action pred init =
    if pred init
       then action init >>= whileDo action pred
       else return init

clamp :: Int -> Int -> Int -> Int
clamp val lower upper
  | val < lower = lower
  | val > upper = upper
  | otherwise = val

