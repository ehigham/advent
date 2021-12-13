module SegmentDisplaySpec ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Day8.SegmentDisplay


tests :: TestTree
tests = testGroup "SegmentDisplay (Day8)"
    [ testShowRead ]


testShowRead = testCase "show . read == id" $
    show (read input :: Entry) @?= input
  where
    input :: String
    input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
