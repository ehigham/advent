module DiveSpec ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Day2.Dive ( Command ( .. ) , Direction ( .. ) )

tests :: TestTree
tests = testGroup "Dive unit tests" [ testRead ]

testRead = testGroup "Testing instance Read Command"
    [   testCase "forward" $ read "forward 5" @?= Command Forward 5
    ,   testCase "down"    $ read "down 8"    @?= Command Down 8
    ,   testCase "up"      $ read "up 3"      @?= Command Up 3
    ]
