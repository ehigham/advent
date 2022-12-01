import Options.Applicative

import Control.Monad (join)

import Advent.Day1 qualified as Day1

main :: IO ()
main = join $ execParser (info (dayN <**> helper) description)
  where
    description = fullDesc
      <> progDesc "Solutions to Advent of Code 2022"
      <> footer "Copyright (C) 2022 Edmund Higham <edhigham@gmail.com>"

    dayN = subparser $ mkCommand "day1" Day1.main

    mkCommand name main' = command name
        (info (main' <$> fileArg name <**> helper) idm)

    fileArg name = argument str
        (  metavar "FILENAME"
        <> help ("Path to file containing " ++ name ++ "'s input data.")
        )
