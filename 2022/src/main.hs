import Options.Applicative

import Control.Monad (join)

import Advent.Day1 qualified as Day1
import Advent.Day2 qualified as Day2
import Advent.Day3 qualified as Day3
import Advent.Day4 qualified as Day4
import Advent.Day5 qualified as Day5


main :: IO ()
main = join $ execParser (info (dayN <**> helper) description)
  where
    description = fullDesc
      <> progDesc "Solutions to https://adventofcode.com/2022"
      <> footer "Copyright (C) 2022 Edmund Higham <edhigham@gmail.com>"

    dayN = subparser $ foldl1 (<>)
      [ mkCommand "day1" Day1.main
      , mkCommand "day2" Day2.main
      , mkCommand "day3" Day3.main
      , mkCommand "day4" Day4.main
      , mkCommand "day5" Day5.main
      ]

    mkCommand name main' = command name
        (info (main' <$> fileArg name <**> helper) idm)

    fileArg name = argument str
        (  metavar "FILENAME"
        <> help ("Path to file containing " ++ name ++ "'s input data.")
        )
