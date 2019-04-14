{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.HUnit (assertEqual, Counts, runTestTT, Test(TestCase, TestList))
import Test.HUnit.Lang (Assertion)
import Math (applyGPDF, autoGPDF, gaussianPDF, mean, std)
import Model (classify, train)

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
        "std -> Just 0.8164966"
        (std (3 :: Int) (0 :: Int) xs)
        (Just 0.8164966 :: Maybe Float)
    , assertEqual
        "std -> Just 1"
        (std (3 :: Int) (1 :: Int) xs)
        (Just 1 :: Maybe Float)
    , assertEqual
        "std -> Nothing"
        (std (1 :: Int) (1 :: Int) $ replicate n' x)
        Nothing
    ]
  where
    x = 0 :: Float
    xs = [-1, 0, 1] :: [Float]
    n' = 5 :: Int

testGaussianPDF :: [Assertion]
testGaussianPDF =
    [ assertEqual
        "gaussianPDF -> Just _"
        (gaussianPDF mu sigma' x)
        (Just 0.24197073 :: Maybe Float)
    , assertEqual
        "gaussianPDF -> Nothing"
        (gaussianPDF mu sigma x)
        Nothing
    ]
  where
    mu = 10 :: Float
    sigma = 0 :: Float
    sigma' = 1 :: Float
    x = 9 :: Float

testApplyGPDF :: [Assertion]
testApplyGPDF =
    [ assertEqual
        {- $ R
           > xs = c(1, 2, 3, 4, 5)
           > dnorm(2.5, mean(xs), sd(xs)) -}
        "applyGPDF -> Just _"
        (applyGPDF (2.5 :: Float) [1 .. 5])
        (Just 0.2400078)
    , assertEqual
        {- $ R
           > xs = c(1, 2)
           > dnorm(1.5, mean(xs), sd(xs)) -}
        "applyGPDF -> Just _"
        (applyGPDF (1.5 :: Float) [1, 2])
        (Just 0.5641896)
    , assertEqual
        "applyGPDF -> Nothing"
        (applyGPDF (2.5 :: Float) [1])
        Nothing
    , assertEqual
        "applyGPDF -> Nothing"
        (applyGPDF (2.5 :: Float) [])
        Nothing
    ]

testAutoGPDF :: [Assertion]
testAutoGPDF =
    [ assertEqual
        "autoGPDF -> Just _"
        (autoGPDF xs)
        (Just ys)
    , assertEqual
        "autoGPDF -> Nothing"
        (autoGPDF [1 :: Float])
        Nothing
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

testTrain :: [Assertion]
testTrain =
    [ assertEqual
        "train -> Just _"
        {- $ R
           > xs = c(-1, 1)
           > dnorm(2.5, mean(xs), sd(xs)) -}
        (train applyGPDF [0 :: Float] [(False, [-1]), (False, [1])])
        (Just (False, 0.2820948))
    , assertEqual
        "train -> Nothing"
        (train applyGPDF [0 :: Float] [(False, [-1])])
        Nothing
    , assertEqual
        "train -> Nothing"
        (train applyGPDF ([] :: [Float]) [(False, [-1])])
        Nothing
    , assertEqual
        "train -> Nothing"
        (train applyGPDF [0 :: Float] [(False, [])])
        Nothing
    , assertEqual
        "train -> Nothing"
        (train applyGPDF ([] :: [Float]) [(False, [])])
        Nothing
    ]

testClassify :: [Assertion]
testClassify =
    [ assertEqual
        "classify -> Just _"
        (classify applyGPDF [0 :: Float] xs)
        (Just 0.119202904)
    , assertEqual
        "classify -> Just _"
        (classify applyGPDF [2 :: Float] xs)
        (Just 0.99752736)
    , assertEqual
        "classify -> Nothing"
        (classify applyGPDF [0 :: Float] (take 2 xs))
        Nothing
    ]
  where
    xs = [(False, [-1]), (True, [1]), (False, [0]), (True, [2])]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) tests
  where
    tests =
        testMean
        ++ testStd
        ++ testGaussianPDF
        ++ testApplyGPDF
        ++ testAutoGPDF
        ++ testTrain
        ++ testClassify
