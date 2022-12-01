import Options.Applicative

import Control.Monad (join)

import Advent.Day1 qualified as Day1

main :: IO ()
main = join $ execParser (info (dayN <**> helper) description)
  where
    description = fullDesc
      <> progDesc "Solutions to Avent of Code 2022"
      <> footer "Copyright (C) 2022 Edmund Higham <edhigham@gmail.com>"

    dayN = subparser $
      command "day1" (info (Day1.main <$> fileInput ()) idm)

    fileInput () = argument str
        (metavar "FILENAME" <> help "Path to file containing input data")
