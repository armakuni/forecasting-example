{-# LANGUAGE NamedFieldPuns #-}

module MonteCarlo
    ( Size(..)
    , Statistics(..)
    , Backlog(..)
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

data Backlog = Backlog
    { stories :: [Size]
    , statistics :: Map Size Statistics
    }

run :: Int -> [Int] -> Backlog -> IO [Float]
run iterations percentiles backlog = do
    results <- simulate iterations backlog
    return $ map (nthPercentile results) percentiles

simulate :: Int -> Backlog -> IO [Float]
simulate iterations backlog =
    sort <$> replicateM iterations (simulateOnce backlog)

simulateOnce :: Backlog -> IO Float
simulateOnce Backlog{stories, statistics} = do
    inputs <- randomNumbers $ length stories
    let backlogStats = map (statistics!) stories
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
