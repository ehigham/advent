import Options.Applicative

import Control.Monad (forM_, join)

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
import Advent.Day11 qualified as Day11
import Advent.Day12 qualified as Day12
import Advent.Day13 qualified as Day13
import Advent.Day14 qualified as Day14


days :: [(String, String, FilePath -> IO ())]
days = [ ( "day1",   Day1.desc,  Day1.main)
       , ( "day2",   Day2.desc,  Day2.main)
       , ( "day3",   Day3.desc,  Day3.main)
       , ( "day4",   Day4.desc,  Day4.main)
       , ( "day5",   Day5.desc,  Day5.main)
       , ( "day6",   Day6.desc,  Day6.main)
       , ( "day7",   Day7.desc,  Day7.main)
       , ( "day8",   Day8.desc,  Day8.main)
       , ( "day9",   Day9.desc,  Day9.main)
       , ("day10",  Day10.desc, Day10.main)
       , ("day11",  Day11.desc, Day11.main)
       , ("day12",  Day12.desc, Day12.main)
       , ("day13",  Day13.desc, Day13.main)
       , ("day14",  Day14.desc, Day14.main)
       ]


main :: IO ()
main = join $ execParser (info (commands <**> helper) description)
  where
    description = fullDesc
      <> progDesc "Solutions to https://adventofcode.com/2022"
      <> footer "Copyright (C) 2022 Edmund Higham <edhigham@gmail.com>"

    commands = subparser
        $ foldMap mkDayCommand days
       <> command "all" (info (pure runall <**> helper) (progDesc "run all"))

    mkDayCommand (name, desc, main') = command name
        (info (main' <$> fileArg name <**> helper) (progDesc desc))

    fileArg name = argument str
        (  metavar "FILENAME"
        <> help ("Path to file containing " ++ name ++ "'s input data.")
        )

    runall = forM_ days $ \(day, desc, main') -> do
        putStrLn $ "--- " ++ desc ++ " ---"
        main' $ "data/" ++ day
        putStrLn ""
