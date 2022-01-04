module Day13.Origami (
        Fold ( FoldX, FoldY )
    ,   Point
    ,   parseInstructions
    ,   runFold
    ) where


import Control.Arrow                 ( first, second )
import Data.Functor                  ( (<&>) )
import Data.List                     ( partition, union )
import Text.Parsec                   ( ParseError, eof, parse )
import Text.Parsec.Char              ( char, digit, newline, string )
import Text.Parsec.String            ( Parser )
import Text.ParserCombinators.Parsec ( choice, sepEndBy, many1 )


type Point = (Int, Int)

data Fold = FoldX Int | FoldY Int
    deriving stock (Eq, Show)


parseInstructions :: String -> Either ParseError ([Point], [Fold])
parseInstructions = parse (instructions <* eof) ""
  where
    instructions = do
        points <- point `sepEndBy` newline
        _ <- newline
        folds  <- fold `sepEndBy` newline
        return (points, folds)

    point = do
        x <- natural
        _ <- char ','
        y <- natural
        return (x, y)

    fold = do
        _ <- string "fold along "
        d <- direction
        _ <- char '='
        d <$> natural

    direction :: Parser (Int -> Fold)
    direction = choice [ char 'x' >> return FoldX
                       , char 'y' >> return FoldY
                       ]

    natural :: Parser Int
    natural = many1 digit <&> read


runFold :: [Point] -> Fold -> [Point]
runFold points = go
  where
    go (FoldX k) = let (hi, lo) = partition ((> k) . fst) points in
        union lo (first (2 * k -) <$> hi)

    go (FoldY k) = let (hi, lo) = partition ((> k) . snd) points in
        union lo (second (2 * k -) <$> hi)
