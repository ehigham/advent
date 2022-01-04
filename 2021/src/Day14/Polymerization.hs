module Day14.Polymerization (
            PolymerFormula ( PolymerFormula, template, insertionRules )
        ,   parsePolymerFormula
        ,   insert
        ) where

import           Control.Monad            ( (>=>) )
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict      ( HashMap )
import           Data.Maybe               ( fromMaybe )
import           Text.Parsec              ( ParseError, parse )
import           Text.Parsec.Char         ( upper, newline, spaces, string )
import           Text.Parsec.Combinator   ( count, eof, many1, manyTill, sepEndBy )
import           Text.Parsec.String       ( Parser )


data PolymerFormula = PolymerFormula {
        template :: String
    ,   insertionRules :: HashMap String String
    }
        deriving stock (Eq, Show)


parsePolymerFormula :: String -> Either ParseError PolymerFormula
parsePolymerFormula = parse (formula <* eof) ""
  where
    formula = do
        template <- manyTill upper newline
        _ <- newline
        rules <- insertionRule `sepEndBy` newline
        return $ PolymerFormula template (M.fromList rules)

    insertionRule ::  Parser (String, String)
    insertionRule = do
        a <- count 2 upper
        _ <- spaces >> string "->" >> spaces
        (a,) <$> many1 upper


insert :: HashMap String String -> String -> String
insert rules = interleave <*> (zipWith list <*> tail >=> replace)
  where
    list a b = [a, b]

    replace = fromMaybe "" . (`M.lookup` rules)

    interleave [] ys = ys
    interleave xs [] = xs
    interleave (x:xs) ys = x : interleave ys xs
