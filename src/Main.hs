{-# OPTIONS_GHC -Wall #-}

module Main where

import Math (applyGPDF, classify)
import Grid (bounds, twoDim)
import Parse (transform)

classifyGrid :: Float -> [(Bool, [Float])] -> Maybe [(Float, [Float])]
classifyGrid k xs =
    (twoDim . bounds k . map snd) xs
    >>= \xs' -> mapM (classify (flip applyGPDF) xs) xs'
    >>= \ys' -> Just (zip ys' xs')

main :: IO ()
main = getContents >>= mapM_ (mapM print) . transform (classifyGrid 5)
