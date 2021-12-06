import Criterion.Main
import Data.Functor      ( (<&>) )
import Data.List.Split   ( splitOn )
import Day6.Lanternfish  ( Lanternfish, simulate )


main :: IO ()
main = defaultMain [day6]


day6 :: Benchmark
day6 = env setup $ \fish -> bgroup "day6"
        [   bench "simulate 18"  $ whnf (simulate 18) fish
        ,   bench "simulate 80"  $ whnf (simulate 80) fish
        ,   bench "simulate 256" $ whnf (simulate 256) fish
        ]
  where
    setup :: IO [Lanternfish]
    setup = readFile "data/Day6/input" <&> (map read . splitOn ",")
