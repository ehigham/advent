module CavesSpec ( tests ) where


import           Data.Functor
import qualified Data.HashMap.Strict as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           Day12.Caves

tests :: TestTree
tests = testGroup "Caves (Day12)"
    [   testCase "test read" $ read "start-A" @?= Conn start "A"
    ,   testCase "test show" $ (show . readC) "start-A" @?= "start-A"
    ,   testConsCaveSystem
    ,   testFindPathsPart1
    ,   testFindPathsPart2
    ]
  where
    readC :: String -> Connection
    readC = read


testConsCaveSystem = testGroup "test consCaveSystem"
    [   testCase "caves" $ assertEqual ""
            (consCaveSystem [Conn "a" "b"])
            (M.fromList [("a", ["b"]), ("b", ["a"])])
    ,   testCase "merge"       $ assertEqual ""
            (consCaveSystem [Conn "a" "B", Conn "B" "c"])
            (M.fromList [("a", ["B"]), ("c", ["B"]), ("B", ["c", "a"])])
    ]


testFindPathsPart1 = testGroup "test findPaths (part 1)"
    [   testCase "example" $ do
            system <- readSystem "example"
            length (findPaths visitingSmallCavesOnce system) @?= 10
    ,   testCase "larger" $ do
            system <- readSystem "larger-example"
            length (findPaths visitingSmallCavesOnce system) @?= 19
    ,   testCase "largest" $ do
            system <- readSystem "largest-example"
            length (findPaths visitingSmallCavesOnce system) @?= 226
    ]

testFindPathsPart2 = testGroup "test findPaths (part 2)"
    [   testCase "example" $ do
            system <- readSystem "example"
            length (findPaths visitingOneSmallCaveTwice system) @?= 36
    ,   testCase "larger" $ do
            system <- readSystem "larger-example"
            length (findPaths visitingOneSmallCaveTwice system) @?= 103
    ,   testCase "largest" $ do
            system <- readSystem "largest-example"
            length (findPaths visitingOneSmallCaveTwice system) @?= 3509
    ]


readSystem :: String -> IO CaveSystem
readSystem filename = let inputFile = "data/Day12/" ++ filename in
    readFile inputFile <&> lines <&> map read <&> consCaveSystem
