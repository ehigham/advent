module SmokeBasinSpec ( tests ) where

import Data.Array
import Data.Char
import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit

import Day9.SmokeBasin


tests :: TestTree
tests = testGroup "SmokeBasin (Day9)"
    [ testIsLowPoint ]


testIsLowPoint :: TestTree
testIsLowPoint = withResource readExample ignore $ \getHeights ->
    testGroup "test isLowPoint"
        [ testCase "isLowPoint (0, 1)" $ do
            heights <- getHeights
            assertBool "" $ isLowPoint heights (0, 1)
        , testCase "isLowPoint (4, 6)" $ do
            heights <- getHeights
            assertBool "" $ isLowPoint heights (4, 6)
        , testCase "example has 4 low points" $ do
            heights <- getHeights
            assertBool "" . (== 4) . length . filter (isLowPoint heights) $ indices heights
        ]
  where
    readExample = readFile "data/Day9/example"
               <&> map (map digitToInt) . lines
               <&> toHeightMap

    ignore :: a -> IO ()
    ignore = const (return ())

