module Day9.SmokeBasin ( isLowPoint, riskLevel, toHeightMap ) where

import Control.Applicative ( liftA2 )
import Data.Array          ( Array, (!), bounds, listArray, indices )


type Point = (Int, Int)
type HeightMap = Array Point Int


isLowPoint :: HeightMap -> Point -> Bool
isLowPoint heights p = all ((heights ! p <) . (heights !)) (neighbours p)
  where
    neighbours = liftA2 (++) adjX adjY

    adjX (x, y) | x == x0   = [(x + 1, y)]
                | x == xN   = [(x - 1, y)]
                | otherwise = [(x - 1, y), (x + 1, y)]

    adjY (x, y) | y == y0   = [(x, y + 1)]
                | y == yN   = [(x, y - 1)]
                | otherwise = [(x, y - 1), (x, y + 1)]

    ((x0, y0), (xN, yN)) = bounds heights


toHeightMap :: [[Int]] -> HeightMap
toHeightMap xs = listArray (p0, pN) (concat xs)
  where
    p0, pN :: Point
    p0 = (0, 0)
    pN = (length xs - 1, length (head xs) - 1)


riskLevel :: HeightMap -> Int
riskLevel heights = sum $ map ((1 +) . (heights !))
                  $ filter (isLowPoint heights) (indices heights)
