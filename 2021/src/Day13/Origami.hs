module Day13.Origami (
        Point ( P, unP )
    ,   Fold ( FoldX, FoldY )
    ,   parseInstructions
    ,   runFold
    ) where


import Control.Arrow                 ( first, second )
import Data.Functor                  ( (<&>) )
import Data.List                     ( partition, sort, union )
import Text.Parsec                   ( ParseError, eof, parse )
import Text.Parsec.Char              ( char, digit, newline, string )
import Text.Parsec.String            ( Parser )
import Text.ParserCombinators.Parsec ( choice, sepEndBy, many1 )


newtype Point = P { unP :: (Int, Int) }
    deriving newtype (Eq, Ord)
    deriving newtype    Show


data Fold = FoldX Int | FoldY Int
    deriving stock (Eq, Show)


parseInstructions :: String -> Either ParseError ([Point], [Fold])
parseInstructions = (first sort <$>) . parse (instructions <* eof) ""
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
        return $ P (x, y)

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


runFold :: Fold -> [Point] -> [Point]
runFold fold = map P . sort . foldAlong fold . map unP
  where
    foldAlong (FoldX k) ps = let (hi, lo) = partition ((> k) . fst) ps in
        union lo (first (2 * k -) <$> hi)

    foldAlong (FoldY k) ps = let (hi, lo) = partition ((> k) . snd) ps in
        union lo (second (2 * k -) <$> hi)
