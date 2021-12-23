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
neighbours heights = liftA2 (++) adjX adjY
  where
    adjX (x, y) = [(x + k, y) | k <- [-1 | x > x0] ++ [1 | x < xN]]
    adjY (x, y) = [(x, y + k) | k <- [-1 | y > y0] ++ [1 | y < yN]]

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
