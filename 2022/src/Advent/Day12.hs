module Advent.Day12 (desc, main) where

import Control.Applicative       (liftA2, liftA3)
import Control.Monad             (guard, join)
import Control.Monad.Extra       (unlessM)
import Control.Monad.Loops       (whileJust', whileM_)
import Control.Monad.ST          (runST)
import Data.Function             (on)
import Data.List                 (sort)
import Data.PQueue.Min           qualified as MinQ
import Data.Map.Strict           qualified as Map
import Data.STRef                (newSTRef, readSTRef, modifySTRef', writeSTRef)
import Data.Matrix               (Matrix)
import Data.Matrix               qualified as Mat
import Text.Parsec.Char          (letter, newline)
import Text.Parsec.Combinator    (many1, sepEndBy1)
import Text.Parsec.Text          (Parser)
import Text.Printf               (printf)

import Advent.Share.ParsecUtils  (parseFile)


desc :: String
desc = "Day 12: Hill Climbing Algorithm"

-- | Part 1
--
-- You try contacting the Elves using your handheld device, but the river you're
-- following must be too low to get a decent signal.
--
-- You ask the device for a heightmap of the surrounding area (your puzzle
-- input). The heightmap shows the local area from above broken into a grid; the
-- elevation of each square of the grid is given by a single lowercase letter,
-- where a is the lowest elevation, b is the next-lowest, and so on up to the
-- highest elevation, z.
--
-- Also included on the heightmap are marks for your current position (S) and
-- the location that should get the best signal (E). Your current position (S)
-- has elevation a, and the location that should get the best signal (E) has
-- elevation z.
--
-- You'd like to reach E, but to save energy, you should do it in as few steps
-- as possible. During each step, you can move exactly one square up, down,
-- left, or right. To avoid needing to get out your climbing gear, the elevation
-- of the destination square can be at most one higher than the elevation of
-- your current square; that is, if your current elevation is m, you could step
-- to elevation n, but not to elevation o. (This also means that the elevation
-- of the destination square can be much lower than the elevation of your
-- current square.)
--
-- For example:
--
-- @
-- Sabqponm
-- abcryxxl
-- accszExk
-- acctuvwj
-- abdefghi
-- @
--
-- Here, you start in the top-left corner; your goal is near the middle. You
-- could start by moving down or right, but eventually you'll need to head
-- toward the e at the bottom. From there, you can spiral around to the goal:
--
-- @
-- v..v<<<<
-- >v.vv<<^
-- .>vv>E^^
-- ..v>>>^^
-- ..>>>>>^
-- @
--
-- In the above diagram, the symbols indicate whether the path exits each square
-- moving up (^), down (v), left (<), or right (>). The location that should get
-- the best signal is still E, and . marks unvisited squares.
--
-- This path reaches the goal in 31 steps, the fewest possible.
--
-- What is the fewest steps required to move from your current position to the
-- location that should get the best signal?
part1 :: Matrix Elevation -> IO ()
part1 = printf "Fewest steps required to move from S to E = %d\n"
      . length
      . liftA3 shortestPath start finish id
  where
    start  = unsafeIndexOf $ Elevation 'S'
    finish = unsafeIndexOf $ Elevation 'E'

-- | Part 2
--
-- As you walk up the hill, you suspect that the Elves will want to turn this
-- into a hiking trail. The beginning isn't very scenic, though; perhaps you can
-- find a better starting point.
--
-- To maximize exercise while hiking, the trail should start as low as possible:
-- elevation a. The goal is still the square marked E. However, the trail should
-- still be direct, taking the fewest steps to reach its goal. So, you'll need
-- to find the shortest path from any square at elevation a to the square marked
-- E.
--
-- Again consider the example from above:
-- @
-- Sabqponm
-- abcryxxl
-- accszExk
-- acctuvwj
-- abdefghi
-- @
--
-- Now, there are six choices for starting position (five marked a, plus the
-- square marked S that counts as being at elevation a). If you start at the
-- bottom-left square, you can reach the goal most quickly:
--
-- @
-- ...v<<<<
-- ...vv<<^
-- ...v>E^^
-- .>v>>>^^
-- >^>>>>>^
-- @
--
-- This path reaches the goal in only 29 steps, the fewest possible.
--
-- What is the fewest steps required to move starting from any square
-- with elevation a to the location that should get the best signal?
part2 :: Matrix Elevation -> IO ()
part2 = printf "Fewest steps required to move from any a to E = %d\n"
      . liftA3 minShortestPath finish id starts
  where
    starts = indexOf (Elevation 'S') <> indexOf (Elevation 'a')
    finish = unsafeIndexOf $ Elevation 'E'

    minShortestPath finish' mat =
        head
        . dropWhile (== 0)
        . sort
        . map (\start' -> length $ shortestPath start' finish' mat)



unsafeIndexOf :: Eq a => a -> Matrix a -> (Int, Int)
unsafeIndexOf = (head .) . indexOf

indexOf :: Eq a => a -> Matrix a -> [(Int, Int)]
indexOf a m = filter
                ((== a) . (m Mat.!))
                [(i, j) | j <- [1..(Mat.ncols m)], i <- [1..(Mat.nrows m)]]

shortestPath :: Enum a
             => (Int, Int)
             -> (Int, Int)
             -> Matrix a
             -> [(Int, Int)]
shortestPath start finish matrix = dijkstra adjacency start finish
  where
    adjacency :: (Int, Int) -> [(Int, Int)]
    adjacency u@(i,j) = let x = matrix Mat.! u in do
        v@(p, q) <- concat [[(i + k, j), (i, j + k)] | k <- [-1, 1]]
        guard $ case Mat.safeGet p q matrix of
            Just y  -> fromEnum y - fromEnum x <= 1
            Nothing -> False
        return v


newtype PathState a = S (a, Int, Maybe a)
    deriving newtype (Show, Eq)

instance Eq a => Ord (PathState a) where
    compare = compare `on` distance
      where
        distance (S (_, x, _)) = x

dijkstra :: Ord a => (a -> [a]) -> a -> a -> [a]
dijkstra adj start finish = runST $ do
    q <- newSTRef . MinQ.singleton $ S (start, 0, Nothing)
    parents <- newSTRef Map.empty

    whileM_ (not . MinQ.null <$> readSTRef q) $ do
        -- pop the first vertex with the smallest distance
        S (v, distance, u) <- pop q
        -- only visit new vertices
        unlessM (Map.member v <$> readSTRef parents) $ do

            -- set the parent vertex of v
            modifySTRef' parents (Map.insert v u)

            if v == finish
                -- we're done - clear the queue to exit `whileM_`
                then writeSTRef q MinQ.empty

                -- extend push the unvisited (adj v) onto the queue
                else do
                    processed <- readSTRef parents
                    mapM_
                        (push q . S . (, distance + 1, Just v))
                        (filter (`Map.notMember` processed) (adj v))

    -- backtrack from the finish to find the start, if a path exists
    v <- newSTRef finish
    parents' <- readSTRef parents
    reverse <$> whileJust'
        (join . (`Map.lookup` parents') <$> readSTRef v)
        (liftA2 (*>) (writeSTRef v) return)
  where
    pop q = do
        (min', q') <- MinQ.deleteFindMin <$> readSTRef q
        writeSTRef q q'
        return min'

    push q = modifySTRef' q . MinQ.insert


inputParser :: Parser (Matrix Elevation)
inputParser = Mat.fromLists <$> many1 elevation `sepEndBy1` newline
  where
    elevation :: Parser Elevation
    elevation = Elevation <$> letter


newtype Elevation = Elevation Char
  deriving newtype (Show, Eq)

instance Ord Elevation where

    compare = (. fromEnum) . compare . fromEnum

instance Enum Elevation where
    toEnum = Elevation . (['a'..'z'] !!)

    fromEnum (Elevation 'S') = fromEnum (Elevation 'a')
    fromEnum (Elevation 'E') = fromEnum (Elevation 'z')
    fromEnum (Elevation x)   = fromEnum x - 97


main :: FilePath -> IO ()
main inputFile = do
    elevations <- parseFile inputParser inputFile
    putStr "Part 1: " >> part1 elevations
    putStr "Part 2: " >> part2 elevations
