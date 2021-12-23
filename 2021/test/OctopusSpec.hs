module OctopusSpec ( tests ) where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit

import Day11.Octopus


tests :: TestTree
tests = withResource readExample ignore $ \getOctopuses ->
    testGroup "Octopuses (Day11)" [
        testCase "test step" $ do
            octopuses <- getOctopuses
            evalState (replicateM 10 step) octopuses
                @?= [0,35,45,16,8,1,7,24,39,29]
    ]
  where
    readExample = readFile "data/Day11/example"
               <&> map (map digitToInt) . lines
               <&> toOctopusArray

    ignore :: a -> IO ()
    ignore = const (return ())
