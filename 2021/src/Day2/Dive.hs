module Day2.Dive ( Command ( .. ), Horizontal, Depth, Aim ) where

import Text.Read

type Horizontal = Int
type Depth      = Int
type Aim        = Int

data Command = Forward Int
             | Up Int
             | Down Int
    deriving stock Eq


instance Show Command where
    show (Forward x) = "forward " ++ show x
    show (Up x)      = "up " ++ show x
    show (Down x)    = "down " ++ show x


instance Read Command where
    readPrec = do
        Ident direction <- lexP
        x <- step readPrec
        case direction of
            "forward" -> return $ Forward x
            "up"      -> return $ Up x
            "down"    -> return $ Down x
            _         -> pfail
