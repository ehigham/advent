module Day3.BinaryDiagnostics (
      Bit ( .. )
    , BinaryNumber ( .. )
    , toInt
    , mcb
    , lcb
    ) where

import Control.Applicative ( many )
import Data.Bits           ( shiftL )
import Data.Boolean
import Data.Functor        ( (<&>) )
import Text.Read


data Bit = Zero | One
    deriving stock Eq


instance Show Bit where
    show Zero = "0"
    show One  = "1"


instance Read Bit where
    readPrec = bit


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


bit :: ReadPrec Bit
bit = get >>= \case
    '0' -> return Zero
    '1' -> return One
    _   -> pfail


toInt :: [Bit] -> Int
toInt = foldl f 0
  where
    f n Zero = shiftL n 1
    f n One  = shiftL n 1 + 1


mcb :: [Bit] -> Bit
mcb digits = let (zeros, ones) = foldl counts (0, 0) digits in
                if zeros > ones then Zero else One
  where
    counts :: (Int, Int) -> Bit -> (Int, Int)
    counts (zeros, ones) Zero = (zeros + 1, ones)
    counts (zeros, ones) One  = (zeros, ones + 1)


lcb :: [Bit] -> Bit
lcb = notB . mcb


newtype BinaryNumber = BinaryNumber { toBitList :: [Bit] }
    deriving stock Eq


instance Show BinaryNumber where
    show = foldl ((. show) . (++)) "" . toBitList


instance Read BinaryNumber where
    readPrec = many bit <&> BinaryNumber

