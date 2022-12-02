module Advent.Share.ParsecUtils (ParseException(..)) where

import Control.Exception    (Exception)
import GHC.Generics         (Generic)
import Text.Parsec          (ParseError)


newtype ParseException = ParseException ParseError
      deriving newtype (Show, Eq)
      deriving stock Generic
      deriving anyclass Exception
