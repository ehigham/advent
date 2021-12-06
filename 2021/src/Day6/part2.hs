import           Data.List.Split           ( splitOn )
import qualified System.Environment as Env
import           Text.Printf               ( printf )
import           Day6.Lanternfish          ( Lanternfish, simulate )

-- | Part Two
-- Suppose the lanternfish live forever and have unlimited food and space.
-- Would they take over the entire ocean?
--
-- After 256 days in the example above, there would be a total of 26984457539
-- lanternfish!
--
-- How many lanternfish would there be after 256 days?

main :: IO ()
main = do
    [input] <- Env.getArgs
    contents <- readFile input
    let fish = map read (splitOn "," contents) :: [Lanternfish]
    printf "After 256 days, there are %d lanternfish.\n" (simulate 256 fish)