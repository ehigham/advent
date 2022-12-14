module Advent.Share.ParsecUtils
      ( ParseException(..)
      , num
      , parseFile
      , parseFileS
      , runInnerPT
      ) where

import Control.Exception            (Exception, throwIO)
import GHC.Generics                 (Generic)
import Text.Parsec                  ( Consumed(Empty,Consumed)
                                    , ParseError
                                    , ParsecT
                                    , Parsec
                                    , Reply
                                    , State
                                    , Stream
                                    , runParsecT
                                    , runP
                                    )
import Text.Parsec.Char             (digit)
import Text.Parsec.Combinator       (many1)
import Data.Text                    (Text)
import Data.Text.IO                 qualified as T

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


parseFile :: Parsec Text () a -> FilePath -> IO a
parseFile parser = parseFileS parser ()


parseFileS :: Parsec Text u a -> u -> FilePath -> IO a
parseFileS parser state filepath = do
      contents <- T.readFile filepath
      case runP parser state filepath contents of
            Left err     -> throwIO (ParseException err)
            Right result -> pure result


num :: (Num a, Stream s m Char) => ParsecT s u m a
num = fromInteger . read <$> many1 digit
