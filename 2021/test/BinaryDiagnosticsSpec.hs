module BinaryDiagnosticsSpec ( tests ) where

import Data.Boolean
import Test.Tasty
import Test.Tasty.HUnit

import Day3.BinaryDiagnostics ( BinaryDigit ( .. ) )

tests :: TestTree
tests = testGroup "BinaryDiagnositcs unit tests" [ testBoolean ]

testBoolean = testGroup "Testing instance Boolean BinaryDigit"
    [   testCase "true"  $ true         @?= One
    ,   testCase "false" $ false        @?= Zero
    ,   testCase "notB"  $ notB One     @?= Zero
    ,   testCase "&&*"   $ One &&* One  @?= One
    ,   testCase "||*"   $ One ||* Zero @?= One
    ]
