module Day11.Octopus (
        Octopus
    ,   EnergyLevel
    ,   step
    ,   toOctopusArray
    ) where

import Control.Monad              ( (>=>), filterM, when )
import Control.Monad.ST           ( ST, runST )
import Control.Monad.State        ( State, get, put )
import Data.Array.MArray          ( Ix
                                  , MArray ( getBounds )
                                  , readArray
                                  , thaw
                                  , writeArray
                                  )
import Data.Array.ST              ( STArray )
import Data.Array                 ( Array, indices )
import GHC.Arr                    ( unsafeFreezeSTArray )

import Day9.SmokeBasin            ( toHeightMap )


type Octopus = (Int, Int)
type EnergyLevel = Int


toOctopusArray :: [[Int]] -> Array Octopus Int
toOctopusArray = toHeightMap


update :: (MArray a e m, Ix i) => (e -> e) -> a i e -> i -> m e
update f array i = do
    x <- readArray array i
    let y = f x
    writeArray array i y
    return y


grow :: STArray s Octopus EnergyLevel -> Octopus -> ST s ()
grow octopuses o = do
    level <- update (+1) octopuses o
    when (level > 9) $ do
        writeArray octopuses o (-1000)
        neighbours <- getNeighbours octopuses o
        mapM_ (grow octopuses) neighbours


getNeighbours :: MArray a e m => a Octopus e -> Octopus -> m [Octopus]
getNeighbours octopuses (x, y) = getBounds octopuses >>= \((x0, y0), (xN, yN)) ->
    return [ (m, n)
           | m <- x : [x - 1 | x > x0] ++ [x + 1 | x < xN]
           , n <- y : [y - 1 | y > y0] ++ [y + 1 | y < yN]
           , (x, y) /= (m, n)
           ]


step :: State (Array Octopus EnergyLevel) Int
step = do
    octopuses <- get
    let idxs = indices octopuses
        (flashes, octopuses') = runST $ do
            stOctopuses <- thaw octopuses
            mapM_ (grow stOctopuses) idxs
            flashed <- filterM (readArray stOctopuses >=> return . (< 0)) idxs
            mapM_ (\i -> writeArray stOctopuses i 0) flashed
            fmap (length flashed,) (unsafeFreezeSTArray stOctopuses)

    put octopuses'
    return flashes
