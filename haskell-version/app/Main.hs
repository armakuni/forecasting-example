module Main where

import qualified Data.Map as Map
import Data.Map (Map)

import MonteCarlo (Backlog, Size(..), Statistics(..), run)

iterations :: Int
iterations = 1000

percentiles :: [Int]
percentiles = [50, 80, 90, 95]

backlog :: Backlog
backlog =
    [ Small
    , Medium
    , Small
    , Small
    , Small
    , Small
    ]

pastPerfomance :: Map Size Statistics
pastPerfomance =
    Map.fromList
        [ (Small, Statistics {low = 1.2, mode = 2.0, high = 3.0})
        , (Medium, Statistics {low = 1.0, mode = 12.0, high = 24.0})
        ]

main :: IO ()
main = run iterations percentiles backlog pastPerfomance >>= print
