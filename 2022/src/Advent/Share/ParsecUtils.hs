module Advent.Share.ParsecUtils
      ( ParseException(..)
      , num
      , parseFile
      , parseFileT
      , xformParsecT
      ) where

import Control.Exception            (Exception, throw)
import Control.Monad.Identity       (runIdentity)
import GHC.Generics                 (Generic)
import Text.Parsec                  ( Consumed(Empty,Consumed)
                                    , ParseError
                                    , ParsecT
                                    , Parsec
                                    , Stream
                                    , Reply
                                    , mkPT
                                    , runParsecT
                                    , runPT
                                    )
import Text.Parsec.Char             (digit)
import Text.Parsec.Combinator       (many1)
import Data.Text                    (Text)
import Data.Text.IO                 qualified as T

newtype ParseException = ParseException ParseError
      deriving newtype (Show, Eq)
      deriving stock Generic
      deriving anyclass Exception


xformParsecT :: (Monad m, Monad f)
             => (f (Consumed (m (Reply s u a))) -> m (Consumed (m (Reply s u b))))
             -> ParsecT s u f a
             -> ParsecT s u m b
xformParsecT mapK parser = mkPT (mapK . runInnerPT)
  where
    runInnerPT state = do
        consumed <- runParsecT parser state
        case consumed of
            Consumed fa -> Consumed . pure <$> fa
            Empty fa    -> Empty . pure <$> fa


parseFile :: Parsec Text s a -> s -> FilePath -> IO a
parseFile = (((runIdentity <$>) .) .) . parseFileT


parseFileT :: (Monad m, Traversable m)
    => ParsecT Text u m a
    -> u
    -> FilePath
    -> IO (m a)
parseFileT parser state filepath = do
    contents <- T.readFile filepath
    traverse inspect $ runPT parser state filepath contents
  where
    inspect (Left err) = throw (ParseException err)
    inspect (Right a)  = pure a


num :: (Num a, Stream s m Char) => ParsecT s u m a
num = fromInteger . read <$> many1 digit
