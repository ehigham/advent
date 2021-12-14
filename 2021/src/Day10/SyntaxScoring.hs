module Day10.SyntaxScoring (
            ParseError (Expected, Unexpected)
        ,   chunks
        ,   parseChunks
        ) where

import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict      ( HashMap )
import qualified Data.HashSet        as S
import           Data.HashSet             ( HashSet )
import           Data.Stack               ( Stack, stackPush, stackPop )


data ParseError = Expected Char Char | Unexpected String
    deriving stock (Show, Read, Eq)


parseChunks :: String -> Either ParseError (Stack Char, String)
parseChunks = chunks mempty


chunks :: Stack Char -> String -> Either ParseError (Stack Char, String)
chunks stack []     = Right (stack, [])
chunks stack (x:xs)
    | isOpen x  = chunks (stackPush stack x) xs
    | isClose x = case stackPop stack of
            Just (stack', a) -> case M.lookup a closer of
                Just b  -> if x == b then chunks stack' xs
                                     else Left $ Expected b x
                Nothing -> Left $ Unexpected (x:xs)
            Nothing          -> Left $ Unexpected (x:xs)
    | otherwise              = Left $ Unexpected (x:xs)
  where
    isOpen = (`S.member` open)
    isClose = (`S.member` close)


open, close :: HashSet Char
open = S.fromList "([{<"
close = S.fromList ")]}>"


closer :: HashMap Char Char
closer = M.fromList
    [ ('(', ')')
    , ('[', ']')
    , ('{', '}')
    , ('<', '>')
    ]
