module Day14.Polymerization (
            PolymerFormula ( PolymerFormula, template, productions )
        ,   bigramCounts
        ,   differenceOfMceAndLceCounts
        ,   elemCounts
        ,   insert
        ,   parsePolymerFormula
        ) where

import           Control.Arrow            ( first )
import           Control.Applicative      ( liftA2 )
import           Control.DeepSeq          ( NFData )
import           Control.Monad            ( (>=>) )
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict      ( HashMap )
import           Data.Maybe               ( fromMaybe )
import           Data.List                ( sort )
import           GHC.Generics             ( Generic )
import           Text.Parsec              ( ParseError, parse )
import           Text.Parsec.Char         ( upper, newline, spaces, string )
import           Text.Parsec.Combinator   ( count, eof, manyTill, sepEndBy )
import           Text.Parsec.String       ( Parser )


data PolymerFormula = PolymerFormula {
        template :: String
    ,   productions :: HashMap String [String]
    }
        deriving stock (Eq, Show, Generic)


instance NFData PolymerFormula


parsePolymerFormula :: String -> Either ParseError PolymerFormula
parsePolymerFormula = parse (formula <* eof) ""
  where
    formula = do
        template <- manyTill upper newline
        _ <- newline
        rules <- insertionRule `sepEndBy` newline
        return $ PolymerFormula template (M.fromList rules)

    insertionRule ::  Parser (String, [String])
    insertionRule = do
        a <- count 2 upper
        _ <- spaces >> string "->" >> spaces
        b <- upper
        return (a, bigrams (between a b))

    between s = (head s :) . (: tail s)


bigramCounts :: String -> HashMap String Integer
bigramCounts chain = M.fromListWith (+) ((, 1) <$> bigrams chain)


insert :: HashMap String [String] -> HashMap String Integer -> HashMap String Integer
insert rules = M.fromListWith (+) . (M.toList >=> \(b, c) -> map (, c) (produce b))
  where
    produce = liftA2 fromMaybe return (`M.lookup` rules)


differenceOfMceAndLceCounts :: HashMap Char Integer -> Integer
differenceOfMceAndLceCounts = liftA2 subtract head last . sort . M.elems


elemCounts :: String -> HashMap String Integer -> HashMap Char Integer
elemCounts template = M.adjust succ (last template)
                    . M.fromListWith (+)
                    . map (first head)
                    . M.toList


bigrams :: String -> [String]
bigrams = zipWith list <*> tail
  where
    list a b = [a, b]
