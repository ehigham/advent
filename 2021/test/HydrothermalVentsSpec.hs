module HydrothermalVentsSpec ( tests ) where
import Test.Tasty
import Test.Tasty.HUnit

import Day5.HydrothermalVents


tests :: TestTree
tests = testGroup "Hydrothermal Vents unit tests"
    [ testRead
    , testAsPoints
    , testIsDiagonal
    ]

testRead = testGroup "test Read"
    [   testCase "Read Point" $ read "1,2"        @?= Point (1, 2)
    ,   testCase "Read Line"  $ read "1,2 -> 2,3" @?= Line (Point (1, 2), Point (2, 3))
    ]

testAsPoints = testGroup "test asPoints"
    [   testCase "delta x"  $ asPoints (read "0,0->5,0") @?= [Point(i, 0) | i <- [0..5]]
    ,   testCase "delta y"  $ asPoints (read "0,0->0,5") @?= [Point(0, j) | j <- [0..5]]
    ,   testCase "negative" $ asPoints (read "5,0->0,0") @?= [Point(i, 0) | i <- [5,4..0]]
    ]

testIsDiagonal = testGroup "test isDiagnonal"
    [   testCase "horizontal" . assertBool "" $ not (isDiagonalS "0,0 -> 5,0")
    ,   testCase "vertical"   . assertBool "" $ not (isDiagonalS "0,0 -> 0,5")
    ,   testCase "diagonal"   . assertBool "" $ isDiagonalS "0,0 -> 5,5"
    ]
  where
    isDiagonalS = isDiagonal . read