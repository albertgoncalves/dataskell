{-# OPTIONS_GHC -Wall #-}

module Payload where

import Control.Monad (zipWithM)
import Data.List (partition, transpose)
import Data.Maybe (listToMaybe)
import GaussianPDF (gaussianPDF, mean, std)
import Prelude hiding (lookup)
import Utils (mapTuple, seqTuple)

applyGPDF :: (Ord a, Floating a) => a -> [a] -> Maybe a
applyGPDF x xs =
    mean n xs
    >>= \mu -> std n 1 xs
    >>= \sigma -> gaussianPDF mu sigma x
  where
    n = length xs

transform :: (Ord a, Floating a) => [a] -> [([a], b)] -> Maybe (a, b)
transform x = seqTuple . (\(a, b) -> (f x a, listToMaybe b)) . unzip
  where
    f :: (Ord a, Floating a) => [a] -> [[a]] -> Maybe a
    f x' xs = exp . sum . map log <$> (zipWithM applyGPDF x' . transpose) xs

probability :: (Eq a, Floating a) => (a, Bool) -> (a, Bool) -> Maybe a
probability (x, True) (y, False) = Just $ x / (x + y)
probability (y, False) (x, True) = Just $ x / (x + y)
probability (0, _) (0, _) = Nothing
probability _ _ = Nothing

classify
    :: (Eq a, Ord a, Floating a)
    => [([a], Bool)]
    -> [a]
    -> Maybe a
classify xs x = f xs
  where
    f =
        (uncurry probability =<<)
        . seqTuple
        . mapTuple (transform x)
        . partition snd

main :: IO ()
main = (print . classify xs) (fst x)
  where
    xs =
        [ ([0.62657841, 16.849859], True)
        , ([-0.03280966, 16.791390], True)
        , ([0.88748101, 4.655336], True)
        , ([0.65862394, 8.828321], True)
        , ([5.304892, -7.1470574], False)
        , ([1.114964, -0.3882628], False)
        , ([20.660083, 7.2656835], False)
        , ([26.430639, -6.6080169], False)
        , ([40.653835, -1.2885520], False)
        ] :: [([Float], Bool)]
    x = ([1.76782283, 7.797349], True) :: ([Float], Bool)
