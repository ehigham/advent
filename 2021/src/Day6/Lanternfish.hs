module Day6.Lanternfish ( Lanternfish, simulate ) where

simulate :: Int -> [Lanternfish] -> [Lanternfish]
simulate days = (!! days) . iterate (>>= spawn)
  where
    spawn 0 = [6, 8]
    spawn n = [n - 1]

type Lanternfish = Int