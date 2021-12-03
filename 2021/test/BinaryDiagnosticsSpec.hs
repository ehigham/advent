module BinaryDiagnosticsSpec ( tests ) where

import Data.Boolean
import Test.Tasty
import Test.Tasty.HUnit

import Day3.BinaryDiagnostics ( Bit ( .. ), mcb, lcb )

tests :: TestTree
tests = testGroup "BinaryDiagnositcs unit tests"
    [ testBoolean
    , testMcb
    , testLcb
    ]

testBoolean = testGroup "Testing instance Boolean Bit"
    [   testCase "true"  $ true         @?= One
    ,   testCase "false" $ false        @?= Zero
    ,   testCase "notB"  $ notB One     @?= Zero
    ,   testCase "&&*"   $ One &&* One  @?= One
    ,   testCase "||*"   $ One ||* Zero @?= One
    ]

testMcb = testGroup "Testing most common bit"
    [   testCase "All Zeros" $ mcb [Zero, Zero, Zero] @?= Zero
    ,   testCase "All Ones"  $ mcb [One,  One,  One]  @?= One
    ,   testCase "A Zero"    $ mcb [Zero, One,  One]  @?= One
    ,   testCase "A One"     $ mcb [Zero, One,  Zero] @?= Zero
    ,   testCase "tie"       $ mcb [Zero, One]        @?= One
    ]

testLcb = testGroup "Testing least common bit"
    [   testCase "All Zeros" $ lcb [Zero, Zero, Zero] @?= One
    ,   testCase "All Ones"  $ lcb [One,  One,  One]  @?= Zero
    ,   testCase "A Zero"    $ lcb [Zero, One,  One]  @?= Zero
    ,   testCase "A One"     $ lcb [Zero, One,  Zero] @?= One
    ,   testCase "tie"       $ lcb [Zero, One]        @?= Zero
    ]