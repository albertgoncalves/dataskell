{-# OPTIONS_GHC -Wall #-}

module IO where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (pack, splitOn, unpack)
import Text.Read (readMaybe)
import Tuple (seqTuple)

intToBool :: Int -> Maybe Bool
intToBool 0 = Just False
intToBool 1 = Just True
intToBool _ = Nothing

stringToData :: String -> Maybe (Bool, [Float])
stringToData = f . map unpack . splitOn (pack ",") . pack
  where
    f [_] = Nothing
    f (x:xs) = seqTuple (readMaybe x >>= intToBool, mapM readMaybe xs)
    f _ = Nothing

dataToString :: (Float, [Float]) -> String
dataToString (x, xs) = (intercalate "," . map show) (x:xs)

transform :: ([(Bool, [Float])] -> a) -> String -> a
transform = (. mapMaybe stringToData . lines)
