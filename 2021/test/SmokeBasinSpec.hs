module SmokeBasinSpec ( tests ) where

import Data.Char
import Data.Functor
import Data.List ( sortBy )
import Test.Tasty
import Test.Tasty.HUnit

import Day9.SmokeBasin


tests :: TestTree
tests = withResource readExample ignore $ \getHeights ->
    testGroup "SmokeBasin (Day9)" [
        testGroup "test isLowPoint"
            [   testCase "isLowPoint (0, 1)" $ do
                    heights <- getHeights
                    assertBool "" $ isLowPoint heights (0, 1)
            ,   testCase "isLowPoint (4, 6)" $ do
                heights <- getHeights
                assertBool "" $ isLowPoint heights (4, 6)
            ]
        ,   testCase "test lowPoints" $ do
                heights <- getHeights
                assertEqual "Example should have 4 low points"
                    4
                    (length $ lowPoints heights)
        ,   testCase "test bassins" $ do
                heights <- getHeights
                assertEqual "Example should have product of 3 largest basins of 1134"
                    1134
                    ( product
                    . take 3
                    . sortBy (flip compare)
                    . map length
                    $ basins heights
                    )
        ]
  where
    readExample = readFile "data/Day9/example"
               <&> map (map digitToInt) . lines
               <&> toHeightMap

    ignore :: a -> IO ()
    ignore = const (return ())

