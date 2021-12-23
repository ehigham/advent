module Day11.Octopus (
        Octopus
    ,   EnergyLevel
    ,   step
    ,   toOctopusPowerLevels
    ) where

import Control.Monad.ST           ( ST, runST )
import Control.Monad.State        ( State, state )
import Data.Array.MArray          ( Ix
                                  , MArray ( getBounds )
                                  , readArray
                                  , thaw
                                  , writeArray
                                  )
import Data.Array.ST              ( STArray )
import Data.Array                 ( Array, Ix ( range ) )
import GHC.Arr                    ( unsafeFreezeSTArray )

import Day9.SmokeBasin            ( toHeightMap )


type Octopus = (Int, Int)
type EnergyLevel = Int


toOctopusPowerLevels :: [[Int]] -> Array Octopus Int
toOctopusPowerLevels = toHeightMap


update :: (MArray a e m, Ix i) => (e -> e) -> a i e -> i -> m e
update f array i = do
    x <- readArray array i
    let y = f x
    writeArray array i y
    return y


powerUp :: STArray s Octopus EnergyLevel -> Octopus -> ST s [Octopus]
powerUp powerLevels octopus = do
    level <- update (+1) powerLevels octopus
    if level > 9 then flash else return []
  where
    flash = do
        writeArray powerLevels octopus (-1000)
        neighbours <- getNeighbours powerLevels octopus
        flashes <- concat <$> mapM (powerUp powerLevels) neighbours
        return $ octopus : flashes


getNeighbours :: MArray a e m => a Octopus e -> Octopus -> m [Octopus]
getNeighbours array (x, y) = adjacency <$> getBounds array
  where
    adjacency ((x0, y0), (xN, yN)) =
        [ (m, n)
        | m <- x : [x - 1 | x > x0] ++ [x + 1 | x < xN]
        , n <- y : [y - 1 | y > y0] ++ [y + 1 | y < yN]
        , (x, y) /= (m, n)
        ]


step :: State (Array Octopus EnergyLevel) Int
step = state runSTStep
  where
    runSTStep powerLevels = runST $ do
        stPowerLevels <- thaw powerLevels
        bounds <- getBounds stPowerLevels
        flashed <- concat <$> mapM (powerUp stPowerLevels) (range bounds)
        mapM_ (\i -> writeArray stPowerLevels i 0) flashed
        fmap (length flashed,) (unsafeFreezeSTArray stPowerLevels)
