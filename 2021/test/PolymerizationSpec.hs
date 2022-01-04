module PolymerizationSpec ( tests ) where

import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Test.Tasty
import           Test.Tasty.HUnit

import           Day14.Polymerization


tests :: TestTree
tests = testGroup "Polymerization (Day14)"
    [   testParsePoylmerFomula
    ,   testBigramCounts
    ,   testInsert
    ]


testParsePoylmerFomula = testGroup "formula parser"
    [   testCase "empty" $ assertEqual ""
            (parsePolymerFormula "\n\n")
            (Right (PolymerFormula "" M.empty))
    ,   testCase "template" $ assertEqual ""
            (parsePolymerFormula "AAA\n\n")
            (Right (PolymerFormula "AAA" M.empty))
    ,   testCase "rules" $ assertEqual ""
            (parsePolymerFormula "\n\nAA -> B\n")
            (Right (PolymerFormula "" (M.singleton "AA" "B")))
    ]


testBigramCounts = testGroup "test bigramCounts"
    [    testCase "empty"     $ bigramCounts ""    @?= M.empty
    ,    testCase "singleton" $ bigramCounts "A"   @?= M.empty
    ,    testCase "bigram"    $ bigramCounts "AB"  @?= M.singleton "AB" 1
    ,    testCase "trigram"   $ bigramCounts "ABC" @?= M.fromList [("AB", 1), ("BC", 1)]
    ,    testCase "duplicate" $ bigramCounts "AAA" @?= M.fromList [("AA", 2)]
    ]


testInsert = withResource readExample ignore $ \getPolymerFormula ->
    testCase "test insert" $ do
        Right PolymerFormula {..} <- getPolymerFormula
        assertEqual "" [('B', 1749), ('C', 298), ('H', 161), ('N', 865)]
                $ L.sort
                . M.toList
                . elemCounts template
                . (!! 10)
                . L.iterate (insert insertionRules)
                $ bigramCounts template
  where
    readExample = parsePolymerFormula <$> readFile "data/Day14/example"

    ignore :: a -> IO ()
    ignore = const (return ())
