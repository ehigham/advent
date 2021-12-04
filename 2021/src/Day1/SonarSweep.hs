module Day1.SonarSweep (runMain, count ) where
import System.Environment as Env ( getArgs )
import Text.Printf               ( printf )


count :: Eq a => a -> [a] -> Int
count = (length .) . filter . (==)


runMain :: ([Int] -> Int) -> IO ()
runMain part = do
    [inputFile] <- Env.getArgs
    contents <- readFile inputFile
    let measurements = map read (words contents) :: [Int]
    printf "There are %d measurements larger than the previous.\n" (part measurements)
