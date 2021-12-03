module DiveSpec ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Day2.Dive ( Position, Command ( .. ) )

tests :: TestTree
tests = testGroup "Dive unit tests" [ testRead ]

testRead = testGroup "Testing instance Read Command"
    [   testCase "forward" $ runCommand (read "forward 5") origin @?= (5, 0)
    ,   testCase "down"    $ runCommand (read "down 8"   ) origin @?= (0, 8)
    ,   testCase "up"      $ runCommand (read "up 3"     ) origin @?= (0, negate 3)
    ]

origin :: Position
origin = (0, 0)