module LanternfishSpec ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Day6.Lanternfish


tests :: TestTree
tests = testGroup "Lanternfish (Day6)"
    [ testSimulate ]


testSimulate = testGroup "test simulate"
    [   testCase "After 18 days"  $ simulate 18  populationO @?= 26
    ,   testCase "After 80 days"  $ simulate 80  populationO @?= 5934
    ,   testCase "After 256 days" $ simulate 256 populationO @?= 26984457539
    ]
  where
    populationO :: [Lanternfish]
    populationO = [3,4,3,1,2]
