{-# OPTIONS_GHC -Wall #-}

module Utils where

import Data.Function (on)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = uncurry . on (,)

seqTuple :: Monad m => (m a, m b) -> m (a, b)
seqTuple (a, b) = a >>= \a' -> (,) a' <$> b
