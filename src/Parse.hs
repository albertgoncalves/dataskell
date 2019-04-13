{-# OPTIONS_GHC -Wall #-}

module Parse where

import Data.Text (pack, splitOn, unpack)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Utils (seqTuple)

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

transform :: ([(Bool, [Float])] -> a) -> String -> a
transform f = f . mapMaybe stringToData . lines

main :: IO ()
main = getContents >>= mapM_ print . transform id
