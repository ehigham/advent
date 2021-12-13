module Day8.SegmentDisplay ( Entry ( Entry, signals, output ) ) where

import           Control.Monad                   ( replicateM )
import qualified Data.HashSet        as S        ( fromList, member )
import           Data.HashSet                    ( HashSet )
import           GHC.Read                        ( expectP )
import           Text.ParserCombinators.ReadP    ( munch1, skipSpaces )
import           Text.ParserCombinators.ReadPrec ( lift )
import           Text.Read                       ( readPrec, Lexeme ( Punc ) )


segments :: HashSet Char
segments = S.fromList ['a'..'g']


data Entry = Entry { signals :: [String], output :: [String] }
    deriving stock Eq


instance Show Entry where
    show Entry{..} = unwords signals ++ " | " ++ unwords output


instance Read Entry where
    readPrec = do
        signals' <- lift $ replicateM 10 readSegment
        expectP (Punc "|")
        output' <- lift $ replicateM 4 readSegment
        return $ Entry signals' output'
      where
        readSegment = skipSpaces >> munch1 (`S.member` segments)
