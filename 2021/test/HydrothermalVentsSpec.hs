module HydrothermalVentsSpec ( tests ) where
import Test.Tasty
import Test.Tasty.HUnit

import Day5.HydrothermalVents


tests :: TestTree
tests = testGroup "Hydrothermal Vents unit tests"
    [ testRead
    ]

testRead = testGroup "test Read"
    [   testCase "Read Point" $ read "1,2"        @?= Point (1, 2)
    ,   testCase "Read Line"  $ read "1,2 -> 2,3" @?= Line (Point (1, 2), Point (2, 3))
    ]