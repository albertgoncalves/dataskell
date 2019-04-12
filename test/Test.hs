{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.HUnit (assertEqual, Counts, runTestTT, Test(TestCase, TestList))
import Test.HUnit.Lang (Assertion)

import GaussianPDF (autoGPDF, gaussianPDF, mean, std)

testMean :: [Assertion]
testMean =
    [ assertEqual "mean -> Just _" (mean n' $ replicate n' x) (Just x)
    , assertEqual "mean -> Nothing" (mean n'' $ replicate n'' x) Nothing
    ]
  where
    x = 5 :: Float
    n' = 5 :: Int
    n'' = 0 :: Int

testStd :: [Assertion]
testStd =
    [ assertEqual
        "std -> Just 0"
        (std n' (1 :: Int) $ replicate n' x)
        (Just 0)
    , assertEqual
        "std -> Nothing"
        (std (1 :: Int) (1 :: Int) $ replicate n' x)
        Nothing
    , assertEqual
        "std -> Just 1"
        (std (3 :: Int) (1 :: Int) xs)
        (Just 1 :: Maybe Float)
    , assertEqual
        "std -> Just 0.8164966"
        (std (3 :: Int) (0 :: Int) xs)
        (Just 0.8164966 :: Maybe Float)
    ]
  where
    x = 0 :: Float
    xs = [-1, 0, 1] :: [Float]
    n' = 5 :: Int

testGaussianPDF :: [Assertion]
testGaussianPDF =
    [ assertEqual
        "gaussianPDF -> Nothing"
        (gaussianPDF mu sigma x)
        Nothing
    , assertEqual
        "gaussianPDF -> Just _"
        (gaussianPDF mu sigma' x)
        (Just 0.24197073 :: Maybe Float)
    ]
  where
    mu = 10 :: Float
    sigma = 0 :: Float
    sigma' = 1 :: Float
    x = 9 :: Float

testAutoGPDF :: [Assertion]
testAutoGPDF =
    [ assertEqual
        "autoGPDF -> Nothing"
        (autoGPDF [1 :: Float])
        Nothing
    , assertEqual
        "autoGPDF -> Just _"
        (autoGPDF xs)
        (Just ys)
    ]
  where
    xs =
        [ -0.9924723
        , -0.1592349
        , -0.1538372
        , 1.5335746
        , 0.3457412
        , -1.5293901
        , -0.4949839
        , 0.1531161
        , -1.2940066
        , -0.9109157
        ] :: [Float]
    ys =
        [ 0.342472100
        , 0.430574950
        , 0.430027070
        , 0.050729718
        , 0.327796300
        , 0.188808300
        , 0.434666570
        , 0.377307770
        , 0.255953730
        , 0.363553300
        ] :: [Float]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) tests
  where
    tests =
        testMean
        ++ testStd
        ++ testGaussianPDF
        ++ testAutoGPDF
