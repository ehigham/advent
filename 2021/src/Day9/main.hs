import           Control.Monad             ( ap )
import           Data.Char                 ( digitToInt )
import           Data.Functor              ( (<&>) )
import           Data.List                 ( sortBy )
import qualified System.Environment as Env ( getArgs )
import           Text.Printf               ( printf )

import           Day9.SmokeBasin           ( HeightMap
                                           , basins
                                           , lowPoints
                                           , toHeightMap
                                           , riskLevel
                                           )

-- | Day 9: Smoke Basin
-- These caves seem to be lava tubes. Parts are even still volcanically active;
-- small hydrothermal vents release smoke into the caves that slowly settles
-- like rain.
--
-- If you can model how the smoke flows through the caves, you might be able to
-- avoid it and be that much safer. The submarine generates a heightmap of the
-- floor of the nearby caves for you (your puzzle input).
--
-- Smoke flows to the lowest point of the area it's in. For example, consider
-- the following heightmap:
--
-- 2 [1] 9  9  9  4  3  2  1 [0]
-- 3  9  8  7  8  9  4  9  2  1
-- 9  8 [5] 6  7  8  9  8  9  2
-- 8  7  6  7  8  9  6  7  8  9
-- 9  8  9  9  9  6 [5] 6  7  8
--
-- Each number corresponds to the height of a particular location, where 9 is
-- the highest and 0 is the lowest a location can be.
--
-- Your first goal is to find the low points - the locations that are lower than
-- any of its adjacent locations. Most locations have four adjacent locations
-- (up, down, left, and right); locations on the edge or corner of the map have
-- three or two adjacent locations, respectively. (Diagonal locations do not
-- count as adjacent.)
--
-- In the above example, there are four low points, all highlighted: two are in
-- the first row (a 1 and a 0), one is in the third row (a 5), and one is in the
-- bottom row (also a 5). All other locations on the heightmap have some lower
-- adjacent location, and so are not low points.
--
-- The risk level of a low point is 1 plus its height. In the above example, the
-- risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels
-- of all low points in the heightmap is therefore 15.
--
-- Find all of the low points on your heightmap. What is the sum of the risk
-- levels of all low points on your heightmap?
part1 :: HeightMap -> IO ()
part1 heights = printf "Risk level = %d.\n" (sumOfRiskLevels heights)
  where
    sumOfRiskLevels = ap ((sum .) . map . riskLevel) lowPoints


-- | Part Two
-- Next, you need to find the largest basins so you know what areas are most
-- important to avoid.
--
-- A basin is all locations that eventually flow downward to a single low point.
-- Therefore, every low point has a basin, although some basins are very small.
-- Locations of height 9 do not count as being in any basin, and all other
-- locations will always be part of exactly one basin.
--
-- The size of a basin is the number of locations within the basin, including
-- the low point. The example above has four basins.
--
-- The top-left basin, size 3:
--
-- [2][1] 9  9  9  4  3  2  1  0
-- [3] 9  8  7  8  9  4  9  2  1
--  9  8  5  6  7  8  9  8  9  2
--  8  7  6  7  8  9  6  7  8  9
--  9  8  9  9  9  6  5  6  7  8
--
-- The top-right basin, size 9:
--
--  2  1  9  9  9 [4][3][2][1][0]
--  3  9  8  7  8  9 [4] 9 [2][1]
--  9  8  5  6  7  8  9  8  9 [2]
--  8  7  6  7  8  9  6  7  8  9
--  9  8  9  9  9  6  5  6  7  8
--
-- The middle basin, size 14:
--
--  2  1  9  9  9  4  3  2  1  0
--  3  9 [8][7][8] 9  4  9  2  1
--  9 [8][5][6][7][8] 9  8  9  2
-- [8][7][6][7][8] 9  6  7  8  9
--  9 [8] 9  9  9  6  5  6  7  8
--
-- The bottom-right basin, size 9:
--
--  2  1  9  9  9  4  3  2  1  0
--  3  9  8  7  8  9  4  9  2  1
--  9  8  5  6  7  8  9 [8] 9  2
--  8  7  6  7  8  9 [6][7][8] 9
--  9  8  9  9  9 [6][5][6][7][8]
--
-- Find the three largest basins and multiply their sizes together. In the above
-- example, this is 9 * 14 * 9 = 1134.
--
-- What do you get if you multiply together the sizes of the three largest
-- basins?
part2 :: HeightMap -> IO ()
part2 = printf "Product of 3 largest basins = %d.\n"
        . product
        . take 3
        . sortBy (flip compare)
        . map length
        . basins


main :: IO ()
main = do
    [input] <- Env.getArgs
    heights <- readFile input <&> lines <&> map (map digitToInt) <&> toHeightMap
    putStr "Part 1: " >> part1 heights
    putStr "Part 2: " >> part2 heights
