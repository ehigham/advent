module Day4.Bingo (
                    Bingo ( .. )
                  , Board
                  , score
                  , playBingo
                  , readBoards
                  ) where

import Control.Monad       ( ap )
import Control.Monad.State ( State, put, get )

import Data.Bool           ( bool )
import Data.Set            ( Set, delete, fromList, toList )
import Data.List           ( nub, transpose )
import Data.List.Split     ( splitWhen )


data Bingo = Bingo
    deriving stock (Show, Eq)


type Board = [Set Int]


playBingo :: Int -> State Board (Maybe Bingo, (Int, Board))
playBingo ball = do
    board <- get
    let board' = map (delete ball) board
    put board'
    return (isBingo board', (ball, board'))
  where
    isBingo :: Board -> Maybe Bingo
    isBingo = bool Nothing (Just Bingo) . any null


readBoards :: [String] -> [Board]
readBoards = map (map fromList . ap (++) transpose . map (map read . words))
           . filter (not . null)
           . splitWhen null

score :: (Int, Board) -> Int
score (ball, board) = ball * sum (nub (toList =<< board))

