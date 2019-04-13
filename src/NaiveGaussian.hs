{-# OPTIONS_GHC -Wall #-}

module Payload where

import Control.Monad (zipWithM)
import Data.List (partition, transpose)
import Data.Maybe (listToMaybe)
import GaussianPDF (applyGPDF)
import Prelude hiding (lookup)
import Utils (equalLength, mapTuple, seqTuple)

train
    :: (Floating a)
    => (a -> [a] -> Maybe a)
    -> [a]
    -> [(b, [a])]
    -> Maybe (b, a)
train f x = seqTuple . (\(b, a) -> (listToMaybe b, f' x a)) . unzip
  where
    f' x' = (exp . sum . map log <$>) . (zipWithM f x' . transpose)

probability :: (Eq a, Floating a) => (Bool, a) -> (Bool, a) -> Maybe a
probability (True, x) (False, y) = Just $ x / (x + y)
probability (False, y) (True, x) = Just $ x / (x + y)
probability (_, 0) (_, 0) = Nothing
probability _ _ = Nothing

classify
    :: (Floating a, Ord a)
    => (a -> [a] -> Maybe a)
    -> [(Bool, [a])]
    -> [a]
    -> Maybe a
classify f xs x = if equalLength (x:map snd xs) then f' xs else Nothing
  where
    f' =
        (uncurry probability =<<)
        . seqTuple
        . mapTuple (train f x)
        . partition fst

main :: IO ()
main = (print . classify (flip applyGPDF) xs) (snd x)
  where
    xs =
        [ (True, [0.62657841, 16.849859])
        , (True, [-0.03280966, 16.791390])
        , (True, [0.88748101, 4.655336])
        , (True, [0.65862394, 8.828321])
        , (False, [5.304892, -7.1470574])
        , (False, [1.114964, -0.3882628])
        , (False, [20.660083, 7.2656835])
        , (False, [26.430639, -6.6080169])
        , (False, [40.653835, -1.2885520])
        ] :: [(Bool, [Float])]
    x = (True, [1.76782283, 7.797349]) :: (Bool, [Float])
