{-# OPTIONS_GHC -Wall #-}

module Utils where

import Data.Function (on)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = uncurry . on (,)

seqTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
seqTuple (Just x, Just y) = Just (x, y)
seqTuple _ = Nothing
