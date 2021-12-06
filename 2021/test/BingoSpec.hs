module BingoSpec ( tests ) where

import Control.Monad ( ap )
import Data.Set      ( fromList )
import Data.List     ( transpose )

import Test.Tasty
import Test.Tasty.HUnit

import Day4.Bingo ( readBoards )




tests :: TestTree
tests = testGroup "Bingo (Day4)"
    [ testReadBoards
    ]

testReadBoards = testCase "readBoards" $ do
    contents <- readFile "data/Day4/board"
    let boards = readBoards (lines contents)
    assertBool "Boards not empty" . not . null $ boards
    assertEqual "Boards eq" boards [ map fromList
                                   . ap (++) transpose
                                   $ [ [  8, 31, 14, 70, 91 ]
                                     , [ 53, 49, 86, 13, 21 ]
                                     , [ 66, 28, 76, 78, 93 ]
                                     , [ 39, 63, 80, 43, 23 ]
                                     , [ 56, 25, 60, 67, 72 ]
                                     ]
                                   ]