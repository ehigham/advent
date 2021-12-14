module SyntaxScoringSpec ( tests ) where

import Data.Function
import Data.Stack
import Test.Tasty
import Test.Tasty.HUnit

import Day10.SyntaxScoring


tests :: TestTree
tests = testGroup "SyntaxScoring (Day10)"
    [  testParseChunks ]

testParseChunks = testGroup "test parseChunks"
    [ testCase "empty" $ parseChunks ""  ~?~ Right (mempty, [])
    , testCase "eof"   $ parseChunks "[" ~?~ Right (stackFromList "[", [])
    , testCase "expected" $ parseChunks "[)" ~?~ Left (Expected ']' ')')
    , testCase "unexpected" $ parseChunks ")" ~?~ Left (Unexpected ")")
    ]
  where
    (~?~) :: (Show a, HasCallStack) => a -> a -> Assertion
    (~?~) = (@?=) `on` show

    stackFromList = go mempty

    go stack [] = stack
    go stack (x:xs) = go (stackPush stack x) xs
