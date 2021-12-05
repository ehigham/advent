import           Control.Monad.State       ( evalState )
import           Data.Functor              ( (<&>) )
import           Data.Maybe                ( isNothing )
import           Data.List                 ( sortOn )
import           Data.List.Split           ( wordsBy )
import qualified System.Environment as Env
import           Text.Printf               ( printf )

import Day4.Bingo ( Bingo ( Bingo ), Board, playBingo, readBoards, score )

--- | Part Two
-- On the other hand, it might be wise to try a different strategy: let the
-- giant squid win.
--
-- You aren't sure how many bingo boards a giant squid could play at once,
-- so rather than waste time counting its arms, the safe thing to do is to
-- figure out which board will win last and choose that one. That way, no matter
-- which boards it picks, it will win for sure.
--
-- In the above example, the second board is the last to win, which happens
-- after 13 is eventually called and its middle column is completely marked.
-- If you were to keep playing until this point, the second board would have a
-- sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
--
-- Figure out which board will win last. Once it wins, what would its final
-- score be?
lastWinner :: [Int] -> [Board] -> Maybe (Int, Board)
lastWinner balls = lookup (Just Bingo)
                 . concat
                 . sortOn length
                 . map ( dropWhile (isNothing . fst)
                       . evalState (mapM playBingo balls)
                       )


main :: IO ()
main = do
    [input] <- Env.getArgs
    (x:xs) <- readFile input <&> lines
    let balls  = map read (wordsBy (== ',')  x) :: [Int]
        boards = readBoards xs
    printf "score = %d\n" $ maybe 0 score (lastWinner balls boards)