module Advent.Share.ParsecUtils (ParseException(..), runInnerPT) where

import Control.Exception    (Exception)
import GHC.Generics         (Generic)
import Text.Parsec          ( Consumed(Empty,Consumed)
                            , ParseError
                            , ParsecT
                            , Reply
                            , State
                            , runParsecT
                            )


newtype ParseException = ParseException ParseError
      deriving newtype (Show, Eq)
      deriving stock Generic
      deriving anyclass Exception


runInnerPT :: (Monad m, Applicative f)
  => ParsecT s u m a
  -> State s u
  -> m (Consumed (f (Reply s u a)))
runInnerPT parser state = do
      consumed <- runParsecT parser state
      case consumed of
            Consumed fa -> Consumed . pure <$> fa
            Empty fa    -> Empty . pure <$> fa
