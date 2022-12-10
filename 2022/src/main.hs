import Options.Applicative

import Control.Monad (join)

import Advent.Day1  qualified as Day1
import Advent.Day2  qualified as Day2
import Advent.Day3  qualified as Day3
import Advent.Day4  qualified as Day4
import Advent.Day5  qualified as Day5
import Advent.Day6  qualified as Day6
import Advent.Day7  qualified as Day7
import Advent.Day8  qualified as Day8
import Advent.Day9  qualified as Day9
import Advent.Day10 qualified as Day10


main :: IO ()
main = join $ execParser (info (dayN <**> helper) description)
  where
    description = fullDesc
      <> progDesc "Solutions to https://adventofcode.com/2022"
      <> footer "Copyright (C) 2022 Edmund Higham <edhigham@gmail.com>"

    dayN = subparser $ foldl1 (<>)
      [ mkCommand  "day1"  Day1.main
      , mkCommand  "day2"  Day2.main
      , mkCommand  "day3"  Day3.main
      , mkCommand  "day4"  Day4.main
      , mkCommand  "day5"  Day5.main
      , mkCommand  "day6"  Day6.main
      , mkCommand  "day7"  Day7.main
      , mkCommand  "day8"  Day8.main
      , mkCommand  "day9"  Day9.main
      , mkCommand "day10" Day10.main
      ]

    mkCommand name main' = command name
        (info (main' <$> fileArg name <**> helper) idm)

    fileArg name = argument str
        (  metavar "FILENAME"
        <> help ("Path to file containing " ++ name ++ "'s input data.")
        )
