{-# LANGUAGE NamedFieldPuns #-}

module MonteCarlo
    ( Size(..)
    , Statistics(..)
    , Backlog(..)
    , run
    ) where

import Control.Monad (replicateM)
import Data.Map (Map, (!))
import Data.List (sort)
import Control.Monad.Random.Class (MonadRandom(getRandoms))

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

run :: MonadRandom m => Int -> [Int] -> Backlog -> m [Float]
run iterations percentiles backlog =
    nthPercentiles percentiles <$> simulate iterations backlog
    where
        nthPercentiles percentiles items = map (nthPercentile items) percentiles

simulate :: MonadRandom m => Int -> Backlog -> m [Float]
simulate iterations backlog =
    sort <$> replicateM iterations (simulateOnce backlog)

simulateOnce :: MonadRandom m => Backlog -> m Float
simulateOnce Backlog{stories, statistics} = do
    inputs <- randomNumbers $ length stories
    let backlogStats = map (statistics!) stories
    let elapsedTimes = zipWith triangularize backlogStats inputs
    return $ sum elapsedTimes

randomNumbers :: MonadRandom m => Int -> m [Float]
randomNumbers count =
    take count <$> getRandoms
    

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
