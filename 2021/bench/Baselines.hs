import Criterion.Main
import Data.Either          ( rights )
import Data.Functor         ( (<&>) )
import Data.List.Split      ( splitOn )
import Day6.Lanternfish     ( Lanternfish, simulate )
import Day12.Caves          ( CaveSystem
                            , consCaveSystem
                            , findPaths
                            , visitingOneSmallCaveTwice
                            )
import Day14.Polymerization ( PolymerFormula (..)
                            , bigramCounts
                            , elemCounts
                            , parsePolymerFormula
                            , insert
                            )


main :: IO ()
main = defaultMain [day6, day12, day14]


day6 :: Benchmark
day6 = env setup $ \fish -> bgroup "day6"
        [   bench "simulate 18"  $ whnf (simulate 18) fish
        ,   bench "simulate 80"  $ whnf (simulate 80) fish
        ,   bench "simulate 256" $ whnf (simulate 256) fish
        ]
  where
    setup :: IO [Lanternfish]
    setup = readFile "data/Day6/input" <&> (map read . splitOn ",")


day12 :: Benchmark
day12 = bgroup "day12" $ map mkbench [ "example"
                                     , "larger-example"
                                     , "largest-example"
                                     ]
  where
    mkbench :: String -> Benchmark
    mkbench name = env (setup name)
                 $ bench name . nf (findPaths visitingOneSmallCaveTwice)


    setup :: String -> IO CaveSystem
    setup filename = let inputFile = "data/Day12/" ++ filename in
                readFile inputFile <&> lines <&> map read <&> consCaveSystem


day14 :: Benchmark
day14 = bgroup "day14" $ map mkbench [ "example", "input" ]
  where
    mkbench :: String -> Benchmark
    mkbench name = env (setup name) $ bench name . nf runReprunPolymerization

    runReprunPolymerization PolymerFormula{..} =
      elemCounts template
      . (!! 40)
      . iterate (insert insertionRules)
      $ bigramCounts template

    setup :: String -> IO PolymerFormula
    setup filename = let inputFile = "data/Day14/" ++ filename in
                readFile inputFile <&> (head . rights . (:[]) . parsePolymerFormula)
