module OrigamiSpec ( tests ) where


import           Test.Tasty
import           Test.Tasty.HUnit

import           Day13.Origami


tests :: TestTree
tests = testGroup "Origami (Day13)"
    [   testParseInstructions
    ,   testRunFold
    ]


testParseInstructions = testGroup "instruction parser"
    [   testCase "empty" $ parseInstructions "\n"    @?= Right ([], [])
    ,   testCase "point" $ parseInstructions "2,5\n\n" @?= Right ([(2, 5)], [])
    ,   testCase "fold"  $ parseInstructions "\nfold along x=1" @?= Right ([], [FoldX 1])
    ]

testRunFold = withResource readExample ignore $ \getInstructions ->
    testGroup "test runFold"
        [   testCase "FoldY 7" $ do
                Right (ps, _) <- getInstructions
                assertEqual "" 17 (length $ runFold ps (FoldY 7))
        ,   testCase "FoldX 5" $ do
                Right (ps, _) <- getInstructions
                assertEqual "" 16 (length $ foldl runFold ps [FoldX 5, FoldY 7])
        ]
  where
    readExample = parseInstructions <$> readFile "data/Day13/example"

    ignore :: a -> IO ()
    ignore = const (return ())
