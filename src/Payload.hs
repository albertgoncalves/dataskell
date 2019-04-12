{-# OPTIONS_GHC -Wall #-}

module Payload where

import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Maybe (listToMaybe)
import GaussianPDF (mean, std)
import Utils (mapTuple, seqTuple)

meanStd :: Floating a => [a] -> Maybe (a, a)
meanStd xs = (,) <$> mean n xs <*> std n 1 xs
  where
    n = length xs

pipeline
    :: Floating a
    => [((a, a), Bool)]
    -> Maybe [(((a, a), (a, a)), Bool)]
pipeline =
    mapM (uncurry f . unzip)
    . groupBy ((==) `on` snd)
    . sortBy (compare `on` snd)
  where
    f a b = (,) <$> seqTuple (mapTuple meanStd $ unzip a) <*> listToMaybe b

main :: IO ()
main = (print . pipeline) xs
  where
    xs =
        [ ((1.62711933, 19.17994), True)
        , ((0.10916089, 11.02794), False)
        , ((-1.77330493, 18.43419), True)
        , ((0.02429435, -1.9271), True)
        , ((-0.41945872, -0.9709209), True)
        , ((0.07085699, 2.405004), True)
        , ((-1.08370374, 13.10275), False)
        , ((0.74712842, 6.512613), True)
        , ((0.21447752, 16.88026), False)
        , ((-1.78036075, 20.43487), True)
        ] :: [((Float, Float), Bool)]





















