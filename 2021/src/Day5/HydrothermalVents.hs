module Day5.HydrothermalVents (
        Line ( Line )
    ,   Point ( Point )
    ,   asPoints
    ,   countPoints
    ,   isDiagonal
    ) where

import           Data.Hashable            ( Hashable )
import qualified Data.HashMap.Strict as M
import           GHC.Read                 ( Read ( readPrec ), expectP )
import           Text.Read                ( Lexeme ( Punc ) )


newtype Point = Point (Int, Int)
    deriving stock   Eq
    deriving newtype Hashable


instance Show Point where
    show (Point (x, y)) = show x ++ "," ++ show y


instance Read Point where
    readPrec = do
        x <- readPrec
        expectP (Punc ",")
        y <- readPrec
        return $ Point (x, y)


newtype Line = Line (Point, Point)
    deriving stock Eq


instance Show Line where
    show (Line (p1, p2)) = show p1 ++ " -> " ++ show p2


instance Read Line where
    readPrec = do
        p1 <- readPrec
        expectP (Punc "->")
        p2 <- readPrec
        return $ Line (p1, p2)


countPoints :: [Line] -> M.HashMap Point Int
countPoints = foldl upsertCount M.empty . (asPoints =<<)
  where
    upsertCount points p = M.insertWith (const succ) p 1 points


isDiagonal :: Line -> Bool
isDiagonal (Line (Point (x1, y1), Point (x2, y2))) = x1 /= x2 && y1 /= y2


asPoints :: Line -> [Point]
asPoints l@(Line (Point (x1, y1), Point (x2, y2))) =
    let xs = range x1 x2
        ys = range y1 y2
    in
        if isDiagonal l
            then zipWith ((Point .) . (,)) xs ys
            else [Point (i, j) | i <- xs, j <- ys]
  where
    range x y = if y > x then [x..y] else [y..x]
