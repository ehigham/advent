import qualified System.Environment as Env ( getArgs )
import Text.Printf ( printf )

import Day2.Dive ( Command, part1 )

main :: IO ()
main = do
    [input] <- Env.getArgs
    contents <- readFile input
    let commands            = map read (lines contents) :: [Command]
        (horizontal, depth) = foldl (flip part1) (0, 0) commands
    printf "Position:\n\thorizontal = %d\n\tdepth = %d\n\tproduct = %d\n"
        horizontal
        depth
        (horizontal * depth)