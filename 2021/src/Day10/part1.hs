import           Control.Applicative       ( (<|>) )
import           Control.Monad             ( (>=>) )
import           Data.Function             ( on )
import           Data.HashMap.Strict       ( HashMap )
import qualified Data.HashMap.Strict as M  ( fromList, lookup )
import           Data.Maybe                ( fromMaybe )
import qualified System.Environment as Env ( getArgs )
import           Text.Parsec               ( Parsec
                                           , between
                                           , char
                                           , eof
                                           , errorPos
                                           , parse
                                           , skipMany
                                           , sourceColumn
                                           )
import           Text.Printf               ( printf )


-- | Day 10: Syntax Scoring
-- You ask the submarine to determine the best route out of the deep-sea cave,
-- but it only replies:
--
-- Syntax error in navigation subsystem on line: all of them
--
-- All of them?! The damage is worse than you thought. You bring up a copy of
-- the navigation subsystem (your puzzle input).
--
-- The navigation subsystem syntax is made of several lines containing chunks.
-- There are one or more chunks on each line, and chunks contain zero or more
-- other chunks. Adjacent chunks are not separated by any delimiter; if one
-- chunk stops, the next chunk (if any) can immediately start. Every chunk must
-- open and close with one of four legal pairs of matching characters:

-- - If a chunk opens with (, it must close with ).
-- - If a chunk opens with [, it must close with ].
-- - If a chunk opens with {, it must close with }.
-- - If a chunk opens with <, it must close with >.
--
-- So, () is a legal chunk that contains no other chunks, as is []. More complex
-- but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and
-- even (((((((((()))))))))).
--
-- Some lines are incomplete, but others are corrupted. Find and discard the
-- corrupted lines first.
--
-- A corrupted line is one where a chunk closes with the wrong character - that
-- is, where the characters it opens and closes with do not form one of the
-- four legal pairs listed above.
--
-- Examples of corrupted chunks include (], {()()()>, (((()))}, and
-- <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its
-- presence causes the whole line to be considered corrupted.
--
-- For example, consider the following navigation subsystem:
--
--  [({(<(())[]>[[{[]{<()<>>
--  [(()[<>])]({[<{<<[]>>(
--  {([(<{}[<>[]}>{[]{[(<()>
--  (((({<>}<{<{<>}{[]{[]{}
--  [[<[([]))<([[{}[[()]]]
--  [{[{({}]{}}([{[{{{}}([]
--  {<[[]]>}<{[{[{[]{()[[[]
--  [<(<(<(<{}))><([]([]()
--  <{([([[(<>()){}]>(<<{{
--  <{([{{}}[<[[[<>{}]]]>[]]
--
-- Some of the lines aren't corrupted, just incomplete; you can ignore these
-- lines for now. The remaining five lines are corrupted:
--
--  - {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
--  - [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
--  - [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
--  - [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
--  - <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
--
-- Stop at the first incorrect closing character on each corrupted line.
--
-- Did you know that syntax checkers actually have contests to see who can get
-- the high score for syntax errors in a file? It's true! To calculate the
-- syntax error score for a line, take the first illegal character on the line
-- and look it up in the following table:
--
--  - ): 3 points.
--  - ]: 57 points.
--  - }: 1197 points.
--  - >: 25137 points.
--
-- In the above example, an illegal ) was found twice (2*3 = 6 points), an
-- illegal ] was found once (57 points), an illegal } was found once (1197
-- points), and an illegal > was found once (25137 points). So, the total syntax
-- error score for this file is 6+57+1197+25137 = 26397 points!
--
-- Find the first illegal character in each corrupted line of the navigation
-- subsystem. What is the total syntax error score for those errors?

illegalCharScore :: HashMap Char Int
illegalCharScore = M.fromList
    [   (')',     3)
    ,   (']',    57)
    ,   ('}',  1197)
    ,   ('>', 25137)
    ]


chunks :: Parsec String () ()
chunks = skipMany $ (between `on` char) '(' ')' chunks
                <|> (between `on` char) '[' ']' chunks
                <|> (between `on` char) '{' '}' chunks
                <|> (between `on` char) '<' '>' chunks


firstIllegalChar :: String -> Maybe Char
firstIllegalChar input = case parse (chunks >> eof) "" input of
    Left err -> let column = sourceColumn (errorPos err) in
                    if column < length input
                        then Just $ input !! (column - 1)
                        else Nothing
    _        -> Nothing


score :: [String] -> Int
score = sum . map (fromMaybe 0 . (firstIllegalChar >=> (`M.lookup` illegalCharScore)))


main :: IO ()
main = do
    [input] <- Env.getArgs
    program <- readFile input
    printf "score: %d.\n" (score (lines program))
