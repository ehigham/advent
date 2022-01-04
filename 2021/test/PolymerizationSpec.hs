module PolymerizationSpec ( tests ) where

import           Control.Applicative
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Test.Tasty
import           Test.Tasty.HUnit

import           Day14.Polymerization

tests :: TestTree
tests = testGroup "Polymerization (Day14)"
    [   testParsePoylmerFomula
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


testInsert = withResource readExample ignore $ \getPolymerFormula ->
    testGroup "test insert"
        [   testCase "step 5" $ do
                Right PolymerFormula {..} <- getPolymerFormula
                let chain = (!! 5) $ iterate (insert insertionRules) template
                assertEqual "length" 97 (length chain)
        ,   testCase "step 10" $ do
                Right PolymerFormula {..} <- getPolymerFormula
                let chain = (!! 10) $ iterate (insert insertionRules) template
                assertEqual "length" 3073 (length chain)
                assertEqual "counts" [('B', 1749), ('C', 298), ('H', 161), ('N', 865)]
                    . map (liftA2 (,) head length)
                    . L.group
                    $ L.sort chain
        ]
  where
    readExample = parsePolymerFormula <$> readFile "data/Day14/example"

    ignore :: a -> IO ()
    ignore = const (return ())
