module Day9.SmokeBasin (
        basins
    ,   lowPoints
    ,   isLowPoint
    ,   riskLevel
    ,   toHeightMap
    ) where

import           Control.Applicative      ( liftA2 )
import           Data.Array               ( Array, (!), bounds, listArray, indices )
import           Data.HashSet             ( HashSet )
import qualified Data.HashSet        as S ( insert, member, singleton, toList )


type Point = (Int, Int)
type HeightMap = Array Point Int


neighbours :: HeightMap -> Point -> [Point]
neighbours heights p = adjX p ++ adjY p
  where
    adjX (x, y) | x == x0   = [(x + 1, y)]
                | x == xN   = [(x - 1, y)]
                | otherwise = [(x - 1, y), (x + 1, y)]

    adjY (x, y) | y == y0   = [(x, y + 1)]
                | y == yN   = [(x, y - 1)]
                | otherwise = [(x, y - 1), (x, y + 1)]

    ((x0, y0), (xN, yN)) = bounds heights


isLowPoint :: HeightMap -> Point -> Bool
isLowPoint heights p = all greaterThanHeightAtP (neighbours heights p)
  where
    greaterThanHeightAtP = (heights ! p <) . (heights !)


lowPoints :: HeightMap -> [Point]
lowPoints = liftA2 filter isLowPoint indices


toHeightMap :: [[Int]] -> HeightMap
toHeightMap xs = listArray (p0, pN) (concat xs)
  where
    p0, pN :: Point
    p0 = (0, 0)
    pN = (length xs - 1, length (head xs) - 1)


riskLevel :: HeightMap  -> Point -> Int
riskLevel = ((1 +) . ) . (!)


basins :: HeightMap -> [[Point]]
basins heights = map (S.toList . converge grow . S.singleton) (lowPoints heights)
  where
    converge :: Eq a => (a -> a) -> a -> a
    converge = until =<< ((==) =<<)

    grow :: HashSet Point -> HashSet Point
    grow s = foldl (flip S.insert) s
        [ p
        | p <- S.toList s >>= neighbours heights
        , not (S.member p s) && heights ! p < 9
        ]
