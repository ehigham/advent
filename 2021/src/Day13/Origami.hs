module Day13.Origami (
        Point ( P )
    ,   Fold ( FoldX, FoldY )
    ,   parseInstructions
    ) where


import Data.Functor                  ( (<&>) )
import Text.Parsec                   ( ParseError, eof, parse )
import Text.Parsec.Char              ( char, digit, newline, string )
import Text.Parsec.String            ( Parser )
import Text.ParserCombinators.Parsec ( choice, sepEndBy, many1 )


newtype Point = P (Int, Int)
    deriving newtype (Eq, Show)


data Fold = FoldX Int | FoldY Int
    deriving stock (Eq, Show)


parseInstructions :: String -> Either ParseError ([Point], [Fold])
parseInstructions = parse instructions ""
  where
    instructions = do
        points <- point `sepEndBy` newline
        _ <- newline
        folds  <- fold `sepEndBy` newline
        eof
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
