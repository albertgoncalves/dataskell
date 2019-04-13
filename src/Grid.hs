{-# OPTIONS_GHC -Wall #-}

module Grid where

import Control.Applicative (liftA2)
import Data.List (transpose)

bounds :: (Enum a, Ord a, RealFrac a) => a -> [[a]] -> [[a]]
bounds k = map f . transpose
  where
    f xs = [n, n + x .. m]
      where
        n = minimum xs
        m = maximum xs
        x = (m - n) / k

twoDim :: Eq a => [[a]] -> Maybe [[a]]
twoDim [x, y] = (Just . map (\(a, b) -> [a, b]) . liftA2 (,) x) y
twoDim _ = Nothing

main :: IO ()
main =  (mapM_ (mapM_ print) . twoDim . bounds 5) xs
  where
    xs =
        [ [0.62657841, 16.849859]
        , [-0.03280966, 16.791390]
        , [0.88748101, 4.655336]
        , [0.65862394, 8.828321]
        , [5.304892, -7.1470574]
        , [1.114964, -0.3882628]
        , [20.660083, 7.2656835]
        , [26.430639, -6.6080169]
        , [40.653835, -1.2885520]
        ] :: [[Float]]
