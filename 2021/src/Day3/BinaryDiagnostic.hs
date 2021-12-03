module Day3.BinaryDiagnostic ( BinaryDigit ( .. ), toInt ) where

import  Data.Bits             (shiftL)
import  Data.Boolean
import  Text.Read


data BinaryDigit = Zero | One
    deriving stock Eq


instance Show BinaryDigit where
    show Zero = "0"
    show One  = "1"


instance Read BinaryDigit where
    readPrec = get >>= \case
        '0' -> return Zero
        '1' -> return One
        _   -> pfail

instance Boolean BinaryDigit where
    true  = One
    false = Zero

    notB One  = Zero
    notB Zero = One

    (&&*) One One = One
    (&&*) _   _   = Zero

    (||*) One _   = One
    (||*) _   One = One
    (||*) _   _   = Zero


toInt :: [BinaryDigit] -> Int
toInt = foldl f 0
  where
    f n Zero = shiftL n 1
    f n One  = shiftL n 1 + 1