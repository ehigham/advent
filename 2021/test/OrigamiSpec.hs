module OrigamiSpec ( tests ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           Day13.Origami


tests :: TestTree
tests = testGroup "Origami (Day13)"
    [   testParseInstructions
    ]


testParseInstructions = testGroup "instruction parser"
    [   testCase "empty" $ parseInstructions "\n"    @?= Right ([], [])
    ,   testCase "point" $ parseInstructions "2,5\n\n" @?= Right ([P (2,5)], [])
    ,   testCase "fold"  $ parseInstructions "\nfold along x=1" @?= Right ([], [FoldX 1])
    ]