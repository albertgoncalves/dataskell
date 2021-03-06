module Math where

mean :: (Integral a, Floating b) => a -> [b] -> Maybe b
mean _ [] = Nothing
mean n xs
    | n <= 0 = Nothing
    | otherwise = Just (sum xs / fromIntegral n)

std :: (Integral a, Floating b) => a -> a -> [b] -> Maybe b
std _ _ [] = Nothing
std _ _ [_] = Nothing
std n d xs =
    sqrt <$> (mean n xs >>= \mean' -> mean (n - d) $ map (f mean') xs)
  where
    f x = (** 2) . (x -)

gaussianPDF :: (Floating a, Ord a) => a -> a -> a -> Maybe a
gaussianPDF mu sigma x
    | sigma <= 0 = Nothing
    | otherwise = Just $ (1 / denom) * expon
  where
    sigma' = sigma ** 2
    expon = (exp . negate) $ ((x - mu) ** 2) / (sigma' * 2)
    denom = sqrt (2 * pi * sigma')

applyGPDF :: (Floating a, Ord a) => a -> [a] -> Maybe a
applyGPDF x xs =
    mean n xs >>= \mu -> std n 1 xs >>= \sigma -> gaussianPDF mu sigma x
  where
    n = length xs

{- $ R
   > xs = ...
   > dnorm(xs, mean(xs), sd(xs)) -}
autoGPDF :: (Ord a, Floating a) => [a] -> Maybe [a]
autoGPDF xs = mapM (`applyGPDF` xs) xs
