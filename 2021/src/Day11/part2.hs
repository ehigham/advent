import           Control.Applicative        ( liftA2 )
import           Control.Monad.State        ( evalState )
import           Data.Array                 ( Array )
import           Data.Char                  ( digitToInt )
import           Data.Functor               ( (<&>) )
import           Data.List                  ( elemIndex )
import           Data.Maybe                 ( fromJust )
import qualified System.Environment  as Env
import           Text.Printf                ( printf )

import           Day11.Octopus              ( Octopus
                                            , EnergyLevel
                                            , toOctopusArray
                                            , step
                                            )



-- | Part 2
-- It seems like the individual flashes aren't bright enough to navigate.
-- However, you might have a better option: the flashes seem to be
-- synchronizing!
--
-- In the example above, the first time all octopuses flash simultaneously
-- is step 195:
--
-- After step 193:
--
-- 5877777777
-- 8877777777
-- 7777777777
-- 7777777777
-- 7777777777
-- 7777777777
-- 7777777777
-- 7777777777
-- 7777777777
-- 7777777777
--
-- After step 194:
--
-- 6988888888
-- 9988888888
-- 8888888888
-- 8888888888
-- 8888888888
-- 8888888888
-- 8888888888
-- 8888888888
-- 8888888888
-- 8888888888
--
-- After step 195:
--
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
-- 0000000000
--
-- If you can calculate the exact moments when the octopuses will all flash
-- simultaneously, you should be able to navigate through the cavern. What
-- is the first step during which all octopuses flash?


firstFlash :: Array Octopus EnergyLevel -> Maybe Int
firstFlash = ((+1) <$>) . liftA2 elemIndex length (evalState (repeatM step))
  where
    repeatM :: Applicative t => t a -> t [a]
    repeatM = sequenceA . repeat


main :: IO ()
main = do
    [input] <- Env.getArgs
    octopuses <- readFile input <&> lines <&> map (map digitToInt) <&> toOctopusArray
    let n = fromJust $ firstFlash octopuses
    printf "Number of flashes after 100 steps %d.\n" n
