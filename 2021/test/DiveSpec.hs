module DiveSpec ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Day2.Dive ( Command ( .. ) )

tests :: TestTree
tests = testGroup "Dive (Day2)" [ testRead, testReadId ]

testRead = testGroup "Testing instance Read Command"
    [   testCase "forward" $ read "forward 5" @?= Forward 5
    ,   testCase "down"    $ read "down 8"    @?= Down 8
    ,   testCase "up"      $ read "up 3"      @?= Up 3
    ]

testReadId = testGroup "Testing show . read = id"
    [ testCase s $ let str = s ++ " 5" in
        (show . (read :: String -> Command)) str @?= str
    | s <- ["forward", "down", "up"]
    ]