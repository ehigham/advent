module Day5.HydrothermalVents (
        Point ( Point )
    ,   Line ( Line )
    ) where

import GHC.Read   ( Read ( readPrec ), expectP )
import Text.Read  ( Lexeme ( Punc ) )

newtype Point = Point (Int, Int)
    deriving stock Eq


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
