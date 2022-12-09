module Advent.Day7 (main) where

import Control.Applicative         ((<|>), liftA2)
import Control.Exception           (throw)
import Control.Monad.RevState      qualified as R
import Control.Monad.Trans         (lift)
import Data.Function               (on)
import Data.Map.Strict             qualified as M
import Data.Maybe                  (fromMaybe)
import Data.Text.IO                qualified as T
import Data.Text                   qualified as T
import Data.Text                   (Text)
import Text.Parsec          hiding ((<|>))
import Text.Printf                 (printf)

import Advent.Share.ParsecUtils    (ParseException(..))


-- Day 7: No Space Left On Device

-- | You can hear birds chirping and raindrops hitting leaves as the expedition
-- proceeds. Occasionally, you can even hear much louder sounds in the distance;
-- how big do the animals get out here, anyway?
--
-- The device the Elves gave you has problems with more than just its
-- communication system. You try to run a system update:
--
-- @
-- $ system-update --please --pretty-please-with-sugar-on-top
-- Error: No space left on device
-- Perhaps you can delete some files to make space for the update?
-- @
--
-- You browse around the filesystem to assess the situation and save the
-- resulting terminal output (your puzzle input). For example:
--
-- @
-- $ cd /
-- $ ls
-- dir a
-- 14848514 b.txt
-- 8504156 c.dat
-- dir d
-- $ cd a
-- $ ls
-- dir e
-- 29116 f
-- 2557 g
-- 62596 h.lst
-- $ cd e
-- $ ls
-- 584 i
-- $ cd ..
-- $ cd ..
-- $ cd d
-- $ ls
-- 4060174 j
-- 8033020 d.log
-- 5626152 d.ext
-- 7214296 k
-- @
--
-- The filesystem consists of a tree of files (plain data) and directories
-- (which can contain other directories or files). The outermost directory is
-- called /. You can navigate around the filesystem, moving into or out of
-- directories and listing the contents of the directory you're currently in.
--
-- Within the terminal output, lines that begin with $ are commands you
-- executed, very much like some modern computers:
--
-- - cd means change directory. This changes which directory is the current
--   directory, but the specific result depends on the argument:
--   - cd x moves in one level: it looks in the current directory for the
--     directory named x and makes it the current directory.
--   - cd .. moves out one level: it finds the directory that contains the
--     current directory, then makes that directory the current directory.
--   - cd / switches the current directory to the outermost directory, /.
-- - ls means list. It prints out all of the files and directories immediately
--   contained by the current directory:
--   - 123 abc means that the current directory contains a file named abc with
--     size 123.
--   - dir xyz means that the current directory contains a directory named xyz.
--
-- Given the commands and output in the example above, you can determine that
-- the filesystem looks visually like this:
--
-- @
-- - / (dir)
--   - a (dir)
--     - e (dir)
--       - i (file, size=584)
--     - f (file, size=29116)
--     - g (file, size=2557)
--     - h.lst (file, size=62596)
--   - b.txt (file, size=14848514)
--   - c.dat (file, size=8504156)
--   - d (dir)
--     - j (file, size=4060174)
--     - d.log (file, size=8033020)
--     - d.ext (file, size=5626152)
--     - k (file, size=7214296)
-- @
--
-- Here, there are four directories: / (the outermost directory), a and d
-- (which are in /), and e (which is in a). These directories also contain files
-- of various sizes.
--
-- Since the disk is full, your first step should probably be to find
-- directories that are good candidates for deletion. To do this, you need to
-- determine the total size of each directory. The total size of a directory is
-- the sum of the sizes of the files it contains, directly or indirectly.
-- (Directories themselves do not count as having any intrinsic size.)
--
-- The total sizes of the directories above can be found as follows:
--
-- - The total size of directory e is 584 because it contains a single file i of
--   size 584 and no other directories.
-- - The directory a has total size 94853 because it contains files f
--   (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly
--   (a contains e which contains i).
-- - Directory d has total size 24933642.
-- - As the outermost directory, / contains every file. Its total size is
--   48381165, the sum of the size of every file.
--
-- To begin, find all of the directories with a total size of at most 100000,
-- then calculate the sum of their total sizes. In the example above, these
-- directories are a and e; the sum of their total sizes is 95437 (94853 + 584).
-- (As in this example, this process can count files more than once!)
--
-- Find all of the directories with a total size of at most 100000. What is the
-- sum of the total sizes of those directories?
part1 :: [File] -> IO ()
part1 = printf "Sum of folder sizes > 100000 = %d\n"
      . sum
      . map bytes
      . filter (liftA2 (&&) isFolder ((<100000) . bytes))
      . filter ((/= "/") . name)


-- | Part 2
part2 :: a -> IO ()
part2 _ = printf "not implemented\n"


data File where
    File :: Text -> Int -> File
    Folder :: Text -> Int -> [File] -> File
  deriving stock (Show)


instance Eq File where
    (==) = (==) `on` name


mkFolder :: Text -> [File] -> File
mkFolder path = liftA2 (Folder path) (foldl ((. bytes) . (+)) 0) id

name :: File -> Text
name (File n _) = n
name (Folder n _ _) = n


bytes :: File -> Int
bytes (File _ b) = b
bytes (Folder _ b _) = b

isFolder :: File -> Bool
isFolder (Folder {}) = True
isFolder _           = False


inputParser :: Parsec Text [Text] [File]
inputParser = mkPT $ \state ->
    let result = R.evalState (p state) M.empty in pure result
  where
    p state = do
        consumed <- runParsecT (concat <$> many command) state
        case consumed of
            Consumed fa -> Consumed . pure <$> fa
            Empty fa    -> Empty . pure <$> fa

    command = string "$ " *> choice
        [ string "cd" *> spaces *> filename >>= ([] <$) . cd
        , string "ls" *> spaces *> (pure <$> ls)
        ]

    cd "/"  = setState ["/"]
    cd ".." = modifyState tail
    cd dir  = modifyState (dir:)

    ls = do
        path <- dirname
        files <- many (file path <|> folder path)
        lift $ R.modify (M.insert path files)
        return $ mkFolder path files

    folder dir = do
        name' <- try (string "dir ") *> filename
        let path = mkPath dir name'
        files <- lift $ R.gets (M.lookup path)
        return $ mkFolder path (fromMaybe [] files)

    file path = flip File
        <$> (read <$> manyTill digit space)
        <*> (mkPath path <$> filename)

    filename = T.pack <$> manyTill anyChar newline

    dirname = foldl1 (flip mkPath) <$> getState

    mkPath "/" = mkPath ""
    mkPath dir = ((dir <> "/") <>)


main :: FilePath -> IO ()
main inputFile = do
    contents <- T.readFile inputFile
    folders <- case runParser inputParser [] inputFile contents of
        Left err -> throw (ParseException err)
        Right folders -> pure folders
    putStr "Part 1: "  >> part1 folders
    putStr "Part 2: "  >> part2 folders
