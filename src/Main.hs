{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (liftA2)
import Data.List (transpose)
import IO (dataToString, transform)
import Math (applyGPDF)
import Model (classify)

bounds :: (Enum a, Floating a, Ord a) => a -> [[a]] -> [[a]]
bounds k = map f . transpose
  where
    f xs = [n, n + x .. m]
      where
        n = minimum xs
        m = maximum xs
        x = (m - n) / k

twoDim :: [[a]] -> Maybe [[a]]
twoDim [x, y] = (Just . map (\(a, b) -> [a, b]) . liftA2 (,) x) y
twoDim _ = Nothing

classifyGrid
    :: (Enum a, Floating a, Ord a)
    => a
    -> [(Bool, [a])]
    -> Maybe [(a, [a])]
classifyGrid k xs =
    (twoDim . bounds k . map snd) xs
    >>= \xs' -> mapM (classify (flip applyGPDF) xs) xs'
    >>= \ys' -> Just (zip ys' xs')

main :: IO ()
main = getContents >>= mapM_ f . transform (classifyGrid 50)
  where
    f = putStrLn . unlines . map dataToString
