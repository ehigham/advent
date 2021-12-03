module Day3.BinaryDiagnostic ( BinaryDigit ( .. ), xor, toInt ) where

import  Data.Bits   (shiftL)
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


xor :: BinaryDigit -> BinaryDigit -> BinaryDigit
xor One  One  = Zero
xor Zero Zero = Zero
xor _    _    = One


toInt :: [BinaryDigit] -> Int
toInt = foldl f 0
  where
    f n Zero = shiftL n 1
    f n One  = shiftL n 1 + 1