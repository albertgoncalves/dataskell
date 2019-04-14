{-# OPTIONS_GHC -Wall #-}

module Model where

import Control.Monad (zipWithM)
import Data.List (partition, transpose)
import Data.Maybe (listToMaybe)
import Tuple (mapTuple, seqTuple)

ratioTrue :: (Eq a, Floating a) => (Bool, a) -> (Bool, a) -> Maybe a
ratioTrue (True, x) (False, y) = Just $ x / (x + y)
ratioTrue (False, y) (True, x) = Just $ x / (x + y)
ratioTrue (_, 0) (_, 0) = Nothing
ratioTrue _ _ = Nothing

equalLength :: [[a]] -> Bool
equalLength = and . (zipWith (==) <*> drop 1) . map length

validate :: [[a]] -> Bool
validate xs@(x:_) = equalLength xs && not (null x)
validate [] = False

train
    :: (Floating a)
    => (a -> [a] -> Maybe a)
    -> [a]
    -> [(b, [a])]
    -> Maybe (b, a)
train f x = seqTuple . (\(b, a) -> (listToMaybe b, f' x a)) . unzip
  where
    f' x' = (exp . sum . map log <$>) . (zipWithM f x' . transpose)

classify
    :: (Floating a, Ord a)
    => (a -> [a] -> Maybe a)
    -> [(Bool, [a])]
    -> [a]
    -> Maybe a
classify f xs x = if validate (x:map snd xs) then f' xs else Nothing
  where
    f' =
        (uncurry ratioTrue =<<)
        . seqTuple
        . mapTuple (train f x)
        . partition fst