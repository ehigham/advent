module Day6.Lanternfish ( Lanternfish, simulate ) where

import           Data.Vector      ( Vector, (//), (!) )
import qualified Data.Vector as V ( generate, head, replicate )


simulate :: Int -> [Lanternfish] -> Int
simulate days = sum . (!! days) . iterate spawn . popCount
  where
    popCount :: [Lanternfish] -> Vector Int
    popCount = foldl (\v age -> v  // [(age, 1 + v ! age)]) (V.replicate 9 0)

    spawn :: Vector Int -> Vector Int
    spawn ages = V.generate 9 nextGen
      where
        nextGen :: Int -> Int
        nextGen 8 = V.head ages
        nextGen 6 = V.head ages + ages ! 7
        nextGen n = ages ! succ n


type Lanternfish = Int