{-# LANGUAGE NamedFieldPuns #-}

module MonteCarlo
    ( Size(..)
    , Statistics(..)
    , Backlog
    , run
    ) where

import System.Random
import Control.Monad (replicateM)
import Data.Map (Map, (!))
import Data.List (sort)

data Size = Small | Medium | Large | XLarge deriving (Ord, Eq, Show)

data Statistics = Statistics
    { low :: Float
    , mode :: Float
    , high :: Float
    } deriving (Show)

type Backlog = [Size]

run :: Int -> [Int] -> Backlog -> Map Size Statistics -> IO [Float]
run iterations percentiles backlog pastPerfomance = do
    results <- simulate iterations backlog pastPerfomance
    return $ map (nthPercentile results) percentiles

simulate :: Int -> Backlog -> Map Size Statistics -> IO [Float]
simulate iterations backlog pastPerfomance =
    sort <$> replicateM iterations (simulateOnce backlog pastPerfomance)

simulateOnce :: Backlog -> Map Size Statistics -> IO Float
simulateOnce backlog pastPerfomance = do
    inputs <- randomNumbers $ length backlog
    let backlogStats = map (pastPerfomance!) backlog
    let elapsedTimes = zipWith triangularize backlogStats inputs
    return $ sum elapsedTimes

randomNumbers :: Int -> IO [Float]
randomNumbers count =
    replicateM count randomIO

nthPercentile :: [a] -> Int  -> a
nthPercentile items percentile =
    items !! desiredIndex
    where
        iterations = length items
        desiredIndex = (percentile * iterations) `div` 100
    
triangularize :: Statistics -> Float -> Float
triangularize Statistics{low, mode, high} value =
    if value <= (mode - low) / (high - low)
        then low + sqrt(value * (high - low) * (mode - low))
        else high - sqrt((1.0 - value) * (high - low) * (high - mode))

