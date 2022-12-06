{-# LANGUAGE NoMonomorphismRestriction #-}
module Advent.Day5 (main) where

import Control.Applicative       ((<|>))
import Control.Monad.ST          (runST)
import Control.Monad             (forM_, replicateM_)
import Control.Exception         (throw)
import Data.Maybe                (listToMaybe)
import Data.Text.IO              qualified as T
import Data.Vector               (Vector)
import Data.Vector               qualified as V
import Data.Vector.Mutable       qualified as Mut
import Text.Parsec               (parse, try)
import Text.Parsec.Char          ( char
                                 , digit
                                 , letter
                                 , spaces
                                 , string
                                 , newline
                                 )
import Text.Parsec.Combinator    (eof, many1, sepBy1, sepEndBy1)
import Text.Parsec.Text          (Parser)
import Text.Printf               (printf)

import Advent.Share.ParsecUtils  (ParseException(..))

-- | Day 5: Supply Stacks
-- The expedition can depart as soon as the final supplies have been unloaded
-- from the ships. Supplies are stored in stacks of marked crates, but because
-- the needed supplies are buried under many other crates, the crates need to be
-- rearranged.
--
-- The ship has a giant cargo crane capable of moving crates between stacks. To
-- ensure none of the crates get crushed or fall over, the crane operator will
-- rearrange them in a series of carefully-planned steps. After the crates are
-- rearranged, the desired crates will be at the top of each stack.
--
-- The Elves don't want to interrupt the crane operator during this delicate
-- procedure, but they forgot to ask her which crate will end up where, and they
-- want to be ready to unload them as soon as possible so they can embark.
--
-- They do, however, have a drawing of the starting stacks of crates and the
-- rearrangement procedure (your puzzle input). For example:
--
-- @
--     [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3
--
-- move 1 from 2 to 1
-- move 3 from 1 to 3
-- move 2 from 2 to 1
-- move 1 from 1 to 2
-- @
--
-- In this example, there are three stacks of crates. Stack 1 contains two
-- crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains
-- three crates; from bottom to top, they are crates M, C, and D. Finally,
-- stack 3 contains a single crate, P.
--
-- Then, the rearrangement procedure is given. In each step of the procedure, a
-- quantity of crates is moved from one stack to a different stack. In the first
-- step of the above rearrangement procedure, one crate is moved from stack 2 to
-- stack 1, resulting in this configuration:
--
-- @
-- [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3
-- @
--
-- In the second step, three crates are moved from stack 1 to stack 3. Crates
-- are moved one at a time, so the first crate to be moved (D) ends up below the
-- second and third crates:
--
-- @
--         [Z]
--         [N]
--     [C] [D]
--     [M] [P]
--  1   2   3
-- @
--
-- Then, both crates are moved from stack 2 to stack 1. Again, because crates
-- are moved one at a time, crate C ends up below crate M:
--
-- @
--         [Z]
--         [N]
-- [M]     [D]
-- [C]     [P]
--  1   2   3
-- @
--
-- Finally, one crate is moved from stack 1 to stack 2:
--
-- @
--         [Z]
--         [N]
--         [D]
-- [C] [M] [P]
--  1   2   3
-- @
--
-- The Elves just need to know which crate will end up on top of each stack; in
-- this example, the top crates are C in stack 1, M in stack 2, and Z in stack
-- 3, so you should combine these together and give the Elves the message CMZ.
--
-- After the rearrangement procedure completes, what crate ends up on top of
-- each stack?
part1 :: Configuration -> [Instruction] -> IO ()
part1 config instrs = printf "Top of stack = '%s'\n" $ runST $ do
    config' <- V.thaw config

    forM_ instrs $ \Move{..} -> replicateM_ quantity $ do
        stack <- Mut.read config' (from - 1)
        Mut.write config' (from - 1) (tail stack)
        Mut.modify config' (head stack:) (to - 1)

    Mut.foldr pop "" config'
  where
    pop stack tops = maybe tops ((:tops) . unCrate) (listToMaybe stack)


-- | Part 2
part2 :: Configuration -> [Instruction] -> IO ()
part2 _ _ = printf "not implemented\n"


inputParser :: Parser (Configuration, [Instruction])
inputParser = (,) <$> config <*> instructions <* eof
  where
    config = do
        layers <- layer `sepEndBy1` newline
        bottoms <- bases
        return . V.fromList $ foldr stack bottoms layers

    instructions = instr `sepEndBy1` spaces

    stack :: [Maybe Crate] -> [Stack Crate] -> [Stack Crate]
    stack ((Just x):xs) (ys:yss) = (x:ys) : stack xs yss
    stack        (_:xs) (ys:yss) = ys : stack xs yss
    stack [] ys = ys
    stack _  [] = error "bottomless stack!"


layer :: Parser [Maybe Crate]
layer = ((Just <$> crate) <|> (Nothing <$ blank)) `sepBy1` char ' '
  where
    crate = char '[' *> (Crate <$> letter) <* char ']'
    blank = try (string "   ")


bases :: Parser [Stack Crate]
bases = char ' ' *> ([] <$ int) `sepEndBy1` spaces

instr :: Parser Instruction
instr = Move
     <$> (string "move " *> int)
     <*> (string " from " *> int)
     <*> (string " to " *> int)


int :: Parser Int
int = read <$> many1 digit


type Configuration = Vector (Stack Crate)


type Stack = []


newtype Crate = Crate { unCrate :: Char }
  deriving newtype (Show, Eq)


data Instruction = Move
    { quantity :: Int
    , from :: Int
    , to :: Int
    }
  deriving stock (Show, Eq)


main :: FilePath -> IO ()
main inputFile = do
    contents <- T.readFile inputFile
    (config, cmds) <- case parse inputParser inputFile contents of
      Left err -> throw (ParseException err)
      Right res  -> pure res
    putStr "Part 1: "  >> part1 config cmds
    putStr "Part 2: "  >> part2 config cmds
