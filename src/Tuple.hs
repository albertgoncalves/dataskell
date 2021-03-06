module Tuple where

import Control.Applicative (liftA2)
import Data.Function (on)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = uncurry . on (,)

seqTuple :: (Applicative f) => (f a, f b) -> f (a, b)
seqTuple = uncurry $ liftA2 (,)
