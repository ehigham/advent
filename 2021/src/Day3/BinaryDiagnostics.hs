module Day3.BinaryDiagnostics ( Bit ( .. ), toInt, mcb, lcb ) where

import  Data.Bits    ( shiftL )
import  Data.List    ( transpose )
import  Data.Boolean
import  Text.Read


data Bit = Zero | One
    deriving stock Eq


instance Show Bit where
    show Zero = "0"
    show One  = "1"


instance Read Bit where
    readPrec = get >>= \case
        '0' -> return Zero
        '1' -> return One
        _   -> pfail


instance Boolean Bit where
    true  = One
    false = Zero

    notB One  = Zero
    notB Zero = One

    (&&*) One One = One
    (&&*) _   _   = Zero

    (||*) One _   = One
    (||*) _   One = One
    (||*) _   _   = Zero



toInt :: [Bit] -> Int
toInt = foldl f 0
  where
    f n Zero = shiftL n 1
    f n One  = shiftL n 1 + 1


mcb :: [[Bit]] -> [Bit]
mcb = map mode . transpose
  where
    mode :: [Bit] -> Bit
    mode digits = let (zeros, ones) = foldl counts (0, 0) digits in
        if zeros > ones then Zero else One

    counts :: (Int, Int) -> Bit -> (Int, Int)
    counts (zeros, ones) Zero = (zeros + 1, ones)
    counts (zeros, ones) One  = (zeros, ones + 1)


lcb :: [[Bit]] -> [Bit]
lcb = map notB . mcb
